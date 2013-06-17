implementation module SemanticAnalyzer

import StdEnv, Parser, TypeChecker, PrettyPrinter, GenEq, AlphaNumIndexed

check :: *UEnv Prog -> (*UEnv, Prog)
check ue [] = (ue, [])
check ue [x:xs] = (nextConsole, [x: nextDecl])
where 
	ue2 = analyze ue x
	(nextConsole, nextDecl) = check ue2 xs

errorsOnly :: *UEnv a -> *UEnv | analyze a
errorsOnly e1 =: { console = console, e = e, error = error1, local = local, global = global } s
# e2 =: { error = error } = (analyze { e1 & console = console, e = e, error = error1, local = local, global = global} s)
= { e1 & console = e2.UEnv.console, error = error, e = e, local = e2.local, global = e2.global, types = e2.types }

idExists :: *UEnv !Id -> *UEnv
idExists e=:{ global = global, local = local } i
	# (result, local) = local getIndexed (i, TEmpty)
	| not (result === TEmpty) = { e & local = local }
	# (result, global) = global getIndexed (i, TEmpty)
	| not (result === TEmpty) = { e & local = local, global = global }
	# (e=:{console = console}, result) = getro { e & local = local, global = global } i
	| not (result === TEmpty) = e
	= { e & error = True, console = console <<< (i +++ " undefined") }

fa :== foldl (analyze)

popro e = { e & ro = tl e.ro }
pushro e ro = { e & ro = [ro:e.ro] }
getro :: *UEnv !Id -> (*UEnv, Type)
getro e=:{ ro = ro } n = (e, foldl (\r (name, type) -> if (name == n) type r) TEmpty ro)

instance analyze Prog where analyze e p = fa e p
instance analyze Decl where	
	analyze e (V v) = analyze e v	
	analyze e (F f) = analyze e f
	analyze e (A a) = analyze e a
instance analyze VarDecl where analyze ue=:{ global = g } v = typeCheck (analyze { ue & global = g addIndexed (v.name, v.type), local = newTree 63 } v.exp) v.exp (toFixed v.type)	
// AlgDecl's are correct when all used parameters, are actually parameters and that all used types are defined
instance analyze AlgDecl 
where analyze ue a = if (foldl (\b part -> b && (foldl (\b2 i -> b2 && isMember i a.poly) True (allIds part.atype))) True a.parts)
			{ ue & types = ue.types add (a.adname, a) }
			{ ue & console = ue.console <<< "Algebraic datatype badly defined", error = True }

//ue // TODO check if used types exists (or are parameters), add type of the type environment
instance analyze FunDecl where
	analyze ue=:{ e = e, global = global, console = console, error = error } f = 
		(fa (foldl (localVar) (returnCheck { ue & e = { e & functionId = Just f.funName }, local = local, global = updatedGlobal, console = console2, error = error || not (old === TEmpty) } f) f.vars) f.stmts) where
		local  = foldl (addIndexed) (newTree 63) [(a.argName, toFixed a.argType) \\ a <- f.args]				
		(old, uGlobal) = global getIndexed (f.funName, TEmpty)
		console2 = console <<< (if (old === TEmpty) "" 
			("ERROR " +++ f.funName +++ " multiply defined on line " +++ (toString f.fline) +++ " column " +++ (toString f.fcolumn) +++ "\n"))
		updatedGlobal = uGlobal addIndexed (f.funName, TFun (f.retType) [a.argType \\ a <- f.args])
		localVar ue=: { local = local } v = { ue & local = local addIndexed (v.name, toFixed v.type) }
instance analyze Stmt where
	analyze e (Block l) = fa e l
	analyze e (If exp stmt) = typeCheck (errorsOnly (errorsOnly e stmt) exp) exp TBool
	analyze e (Ife exp st1 st2) = typeCheck (errorsOnly (errorsOnly (errorsOnly e st2) st1) exp) exp TBool
	analyze e (While exp stmt) = typeCheck (errorsOnly (errorsOnly e stmt) exp) exp TBool
	analyze ue (Ass i exp) = let (t, ue2) = typeFor ue i in typeCheck (idExists ue2 i) exp t
	analyze e (SFC f) = typeCheck (analyze e f) (EFC f) TEmpty
	analyze e (Match name cases) = foldl analyzeCase e cases where
		analyzeCase :: *UEnv Case -> *UEnv
		analyzeCase e (Case c [] stmt) = analyze e stmt
		analyzeCase e (Case c [varname] stmt) = popro (analyze (pushro e2 (varname, atype)) stmt) where (atype, e2) = typeFor e name	
		// TODO check completeness?		
	analyze ue=:{e=e} Return = let (t, ue2) = typeFor ue (fromJust e.functionId) in returnHelp (typeCheck ue2 (returnType t) TEmpty) 
	analyze ue=:{e=e} (Returne exp) = returnHelp (typeCheck (analyze ue2 exp) exp (toFixed (returnType t)))
	where (t, ue2) = typeFor ue ( fromJust e.functionId )
instance analyze Exp where analyze ue=:{ e = e } exp = analyze { ue & e = { e & envLine = exp.eline, envColumn = exp.ecolumn } } exp.ex
instance analyze Exp2 where 
	analyze e (I i) = idExists e i
	analyze e (Op2 e1 op e2) = errorsOnly (errorsOnly e e1) e1
	analyze e (Op1 op exp) = errorsOnly e exp
	analyze e (EBrace exp) = analyze e exp
	analyze e (EFC f) = analyze e f // <- typeChecked as part of a statement
	analyze e (Tup e1 e2) = errorsOnly (errorsOnly e e1) e2
	analyze e (Alg i []) = e
	analyze e (Alg i [exp]) = errorsOnly e exp
	analyze e _ = e	// Bool, Int, Block <- typeCheck as part of statement (assignment or function call)	
instance analyze FunCall where analyze e f = (foldl (errorsOnly) (idExists e f.callName) f.callArgs)

returnHelp ue=:{ e = e, console = console }
| isJust e.functionId = ue
= { ue & console = console <<< "Return used outside of function body" }

returnError :: *UEnv String FunDecl -> *UEnv
returnError ue=:{ e = e, console = console } s f = { UEnv | ue & console = console <<<
	(s +++ (if (isJust e.functionId) (" in function " +++ (fromJust e.functionId)) "") +++ " on line " +++
	(toString f.fline) +++ " column " +++ (toString f.fcolumn) +++ "\n"), e = e, error = True }

returnCheck :: *UEnv FunDecl -> *UEnv
returnCheck e f = let (g, b) = foldl (rtCheck) (e, False) f.stmts in if (b || (isVoid f.retType)) g 
	(returnError g ("Not all branches have a return, or void function with expression in return statement " +++
	(toString f.retType) +++ " " +++ (toString (isVoid f.retType))) f)
where
	rtCheck :: !(!*UEnv, !Bool) Stmt -> (*UEnv, Bool)
	rtCheck (e, True) _ 		= (returnError e "Unreachable code found (statements after return)" f, True)
	rtCheck t (Block stmts) 	= foldl (rtCheck) t stmts
	rtCheck t (If _ stmt) 		= (fst (rtCheck t stmt), False) // An if cannot definitively return, but can throw unreachale errors
	rtCheck t (Ife _ s1 s2) 	= let (e, b1) = (rtCheck t s1) in (let (e2, b2) = (rtCheck (e, False) s2) in (e2, b1 && b2))
	rtCheck t (While _ stmt) 	= (fst (rtCheck t stmt), False) // See if..
	rtCheck (e, _) Return 		= (e, True)
	rtCheck (e, _) (Returne _) 	= (e, not (isVoid f.retType))
	rtCheck t _ 				= t // Assignment, functioncalls
	
derive gEq Type, RetType
