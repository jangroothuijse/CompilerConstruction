implementation module SemanticAnalyzer

import StdEnv
import Tokenizer
import Parser
import TypeChecker
import PrettyPrinter

fa = foldl (analyze)

errorsOnly e s = { e & envErrors = (analyze e s).envErrors }

idExists :: Id Env (Env -> Env) -> Env
idExists i e c = f e.ids
where
	f [] = { e & envErrors = ["Identifier " +++ i +++ " used but not defined" : e.envErrors] }
	f [(x, _):xs] = if (x == i) (c e) (f xs)

typeFor :: Env Id -> Type
typeFor e i = f e.ids i
where 
	f [] i = TEmpty	
	f [(x, xType):xs] i = if (x == i) xType (f xs i)

class analyze a :: Env a -> Env

instance analyze Prog
where analyze e p = fa e p

instance analyze Decl
where	
	analyze e (V v) = analyze e v	
	analyze e (F f) = analyze e f	

instance analyze VarDecl
where analyze e v = { e & ids = [(v.name, v.type) : e.ids], envErrors = (analyze e v.exp).envErrors }
	
instance analyze FunDecl
where 
	analyze e f = { e & ids = ids2, envErrors = (fa (fa { e & ids = argIds ++ ids2, functionId = Just f.funName } f.vars) f.stmts).envErrors }
	where 
		argIds = [(a.argName, a.argType) \\ a <- f.args]
		ids2 = [(f.funName, TFun (f.retType) (map snd argIds)):e.ids]
	
instance analyze Stmt
where
	analyze e (Block l) = fa e l
	analyze e (If exp stmt) = analyzeType (errorsOnly (errorsOnly e stmt) exp) exp TBool
	analyze e (Ife exp st1 st2) = analyzeType (errorsOnly (errorsOnly (errorsOnly e st2) st1) exp) exp TBool
	analyze e (While exp stmt) = analyzeType (errorsOnly (errorsOnly e stmt) exp) exp TBool
	analyze e (Ass i exp) = analyzeType (analyze (idExists i e id) exp) exp (typeFor e i)
	analyze e (SFC f) = analyze e f
	analyze e Return = returnHelp e	(analyzeType e (returnType (typeFor e (fromJust e.functionId))) TEmpty) 
	analyze e (Returne exp) = returnHelp e (analyzeType (analyze e exp) exp (returnType (typeFor e ( fromJust e.functionId))))

returnHelp e f = if (isJust e.functionId) f {e & envErrors = ["Return outside of function":e.envErrors]}

instance analyze Exp
where 
	analyze e (I i) = idExists i e id
	analyze e (Op2 e1 op e2) = errorsOnly (errorsOnly e e1) e1
	analyze e (Op1 op exp) = errorsOnly e exp
	analyze e (EInt i) = e	
	analyze e (EBrace exp) = analyze e exp
	analyze e (EFC f) = analyze e f
	analyze e EBlock = e
	analyze e (Tup e1 e2) = errorsOnly (errorsOnly e e1) e2
	analyze e bool = e
	
instance analyze FunCall
where analyze e f = foldl (errorsOnly) (idExists f.callName e id) f.callArgs
