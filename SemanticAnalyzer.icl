implementation module SemanticAnalyzer

import StdEnv
import Tokenizer
import Parser
import TypeChecker
import PrettyPrinter
import SPLDefaultEnv

errorsOnly e s = { e & envErrors = (analyze e s).envErrors }

idExists :: !Id !Env (!Env -> Env) -> Env
idExists i e c = let f = (\l.if (isEmpty l) { e & envErrors = [i +++ " undefined" : e.envErrors] } (if (fst (hd l) == i) (c e) (f (tl l)))) in f e.ids

typeFor :: !Env !Id -> Type
typeFor e i = let f = (\l.if (isEmpty l) TEmpty (let x = (hd l) in (if (fst x == i) (snd x) (f (tl l))))) in f e.ids

staticAnalyze :: (Result Prog) -> Result (Prog, Env)
staticAnalyze (Res p)
# env=:{envErrors = er}	= analyze splDefaultEnv p
# rc					= returnCheck p
| isEmpty er =	case rc of
				Res _ = Res (p, env)
				Err e = Err e
= case rc of
				Res _ = Err er
				Err e = Err (er ++ e)
staticAnalyze (Err p) = Err p

fa :== foldl (analyze)

instance analyze Prog where analyze e p = fa e p

instance analyze Decl where	
	analyze e (V v) = analyze e v	
	analyze e (F f) = analyze e f	

instance analyze VarDecl where analyze e v = typeCheck (analyze { e & ids = [(v.name, v.type) : e.ids] } v.exp) v.exp v.type
	
instance analyze FunDecl  where 
	analyze e f = { e & ids = ids2, envErrors = (fa (fa { e & ids = fixedArgIds ++ ids2, functionId = Just f.funName } f.vars) f.stmts).envErrors }
	where 
		fixedArgIds  = [(a.argName, toFixed a.argType) \\ a <- f.args]
		ids2 = [(f.funName, TFun (f.retType) [a.argType \\ a <- f.args]):e.ids]
	
instance analyze Stmt where
	analyze e (Block l) = fa e l
	analyze e (If exp stmt) = typeCheck (errorsOnly (errorsOnly e stmt) exp) exp TBool
	analyze e (Ife exp st1 st2) = typeCheck (errorsOnly (errorsOnly (errorsOnly e st2) st1) exp) exp TBool
	analyze e (While exp stmt) = typeCheck (errorsOnly (errorsOnly e stmt) exp) exp TBool
	analyze e (Ass i exp) = typeCheck (idExists i e id) exp (typeFor e i)
	analyze e (SFC f) = typeCheck (analyze e f) (EFC f) TEmpty
	analyze e Return = returnHelp e	(typeCheck e (returnType (typeFor e (fromJust e.functionId))) TEmpty) 
	analyze e (Returne exp) = returnHelp e (typeCheck (analyze e exp) exp (toFixed (returnType (typeFor e ( fromJust e.functionId)))))

returnHelp e f = if (isJust e.functionId) f {e & envErrors = ["Return used outside of function body":e.envErrors]}

instance analyze Exp where analyze e exp = analyze { e & envLine = exp.eline, envColumn = exp.ecolumn } exp.ex

instance analyze Exp2 where 
	analyze e (I i) = idExists i e id
	analyze e (Op2 e1 op e2) = errorsOnly (errorsOnly e e1) e1
	analyze e (Op1 op exp) = errorsOnly e exp
	analyze e (EInt i) = e	
	analyze e (EBrace exp) = analyze e exp
	analyze e (EFC f) = analyze e f // <- typeChecked as part of a statement
	analyze e EBlock = e
	analyze e (Tup e1 e2) = errorsOnly (errorsOnly e e1) e2
	analyze e bool = e
	
instance analyze FunCall where analyze e f = (foldl (errorsOnly) (idExists f.callName e id) f.callArgs)
