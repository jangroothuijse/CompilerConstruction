implementation module SemanticAnalyzer

import StdEnv
import Tokenizer
import Parser
import TypeChecker
import PrettyPrinter

check :: Prog *UEnv -> (Prog, *UEnv)
check [] ue = ([], ue)
check [x:xs] ue = ([x: nextDecl], nextConsole)
where 
	ue2 = analyze ue x
	(nextDecl, nextConsole) = check xs ue2

errorsOnly :: *UEnv a -> *UEnv | analyze a
errorsOnly { console = console, e = e, error = error1 } s
# e2 =: { error = error } = (analyze { console = console, e = e, error = error1 } s)
= { UEnv | console = e2.UEnv.console, error = error, e = e }

idExists :: !Id *UEnv -> *UEnv
idExists i { console = console, e = e, error = error } = { UEnv | console = f e.Env.ids console, e = e, error = error }
where 
	f :: [(Id, Type)] *File -> *File
	f [] c		= c <<< (i +++ " undefined")
	f [x:xs] c	= if (fst x == i) c (f xs c)

typeFor :: Env !Id -> Type
typeFor e i = let f = (\l.if (isEmpty l) TEmpty (let x = (hd l) in (if (fst x == i) (snd x) (f (tl l))))) in f e.ids

fa :== foldl (analyze)

instance analyze Prog where analyze e p = fa e p
instance analyze Decl where	
	analyze e (V v) = analyze e v	
	analyze e (F f) = analyze e f	
instance analyze VarDecl where analyze ue=:{ e = e } v = typeCheck (analyze { ue & e = { e & ids = [(v.name, v.type) : e.ids] } } v.exp) v.exp v.type	
instance analyze FunDecl where
	analyze ue=:{ e = e } f = { ue2 & e = { e & ids = ids2 } }
	where 
		ue2 = (fa (fa (returnCheck { ue & e = { e & ids = fixedArgIds ++ ids2, functionId = Just f.funName } } f) f.vars) f.stmts)
		fixedArgIds  = [(a.argName, toFixed a.argType) \\ a <- f.args]
		ids2 = [(f.funName, TFun (f.retType) [a.argType \\ a <- f.args]):e.ids]	
instance analyze Stmt where
	analyze e (Block l) = fa e l
	analyze e (If exp stmt) = typeCheck (errorsOnly (errorsOnly e stmt) exp) exp TBool
	analyze e (Ife exp st1 st2) = typeCheck (errorsOnly (errorsOnly (errorsOnly e st2) st1) exp) exp TBool
	analyze e (While exp stmt) = typeCheck (errorsOnly (errorsOnly e stmt) exp) exp TBool
	analyze ue=:{ e = e } (Ass i exp) = typeCheck (idExists i ue) exp (typeFor e i)
	analyze e (SFC f) = typeCheck (analyze e f) (EFC f) TEmpty
	analyze ue=:{ e = e } Return = returnHelp (typeCheck ue (returnType (typeFor e (fromJust e.functionId))) TEmpty) 
	analyze ue=:{ e = e } (Returne exp) = returnHelp (typeCheck (analyze ue exp) exp (toFixed (returnType (typeFor e ( fromJust e.functionId)))))
instance analyze Exp where analyze ue=:{ e = e } exp = analyze { ue & e = { e & envLine = exp.eline, envColumn = exp.ecolumn } } exp.ex
instance analyze Exp2 where 
	analyze e (I i) = idExists i e
	analyze e (Op2 e1 op e2) = errorsOnly (errorsOnly e e1) e1
	analyze e (Op1 op exp) = errorsOnly e exp
	analyze e (EBrace exp) = analyze e exp
	analyze e (EFC f) = analyze e f // <- typeChecked as part of a statement
	analyze e (Tup e1 e2) = errorsOnly (errorsOnly e e1) e2
	analyze e _ = e	// Bool, Int, Block <- typeCheck as part of statement (assignment or function call)	
instance analyze FunCall where analyze e f = (foldl (errorsOnly) (idExists f.callName e) f.callArgs)

returnHelp ue=:{ e = e, console = console }
| isJust e.functionId = ue
= { ue & console = console <<< "Return used outside of function body" }

returnError :: *UEnv String -> *UEnv
returnError ue=:{ e = e, console = console } s = { UEnv | ue & console = console <<< (s +++ (if (isJust e.functionId) (" in function " +++ (fromJust e.functionId)) "")+++ "\n"), e = e, error = True }

returnCheck :: *UEnv FunDecl -> *UEnv
returnCheck e f = let (g, b) = foldl (rtCheck) (e, False) f.stmts in if (b || (isVoid f.retType)) g (returnError g ("Not all branches have a return " +++ (toString f.retType) +++ " " +++ (toString (isVoid f.retType))))
where
	rtCheck :: !(!*UEnv, !Bool) Stmt -> (*UEnv, Bool)
	rtCheck (e, True) _ 		= (returnError e "Unreachable code found (statements after return)", True)
	rtCheck t (Block stmts) 	= foldl (rtCheck) t stmts
	rtCheck t (If _ stmt) 		= (fst (rtCheck t stmt), False) // An if cannot definitively return, but can throw unreachale errors
	rtCheck t (Ife _ s1 s2) 	= let (e, b1) = (rtCheck t s1) in (let (e2, b2) = (rtCheck (e, False) s2) in (e2, b1 && b2))
	rtCheck t (While _ stmt) 	= (fst (rtCheck t stmt), False) // See if..
	rtCheck (e, _) Return 		= (e, True)
	rtCheck (e, _) (Returne _) 	= (e, True)
	rtCheck t _ 				= t // Assignment, functioncalls
