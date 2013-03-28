implementation module SemanticAnalyzer

import StdEnv
import Tokenizer
import Parser
import Result

fa = foldl (analyze)
errorsOnly e s = { e & envErrors = (analyze e s).envErrors }
idExists i e = if (isMember i e.ids) e { e & envErrors = ["Identifier " +++ i +++ " used but not defined" : e.envErrors] }
analyzer :: (Prog -> Env)
analyzer = analyze { ids = [], envErrors = [] }

class analyze a :: Env a -> Env

instance analyze Prog
where analyze e p = fa e p

instance analyze Decl
where	
	analyze e (V v) = analyze e v	
	analyze e (F f) = analyze e f	

instance analyze VarDecl
where analyze e v = { ids = [v.name : e.ids], envErrors = (analyze e v.exp).envErrors }
	
instance analyze FunDecl
where analyze e f = { ids = [f.funName : e.ids], envErrors = (fa (fa { e & ids = [a.argName \\ a <- f.args] ++ e.ids } f.vars) f.stmts).envErrors }
	
instance analyze Stmt
where
	analyze e (Block l) = fa e l
	analyze e (If exp stmt) = errorsOnly (errorsOnly e stmt) exp
	analyze e (Ife exp st1 st2) = errorsOnly (errorsOnly (errorsOnly e st2) st1) exp
	analyze e (While exp stmt) = errorsOnly (errorsOnly e stmt) exp
	analyze e (Ass i exp) = analyze (idExists i e) exp
	analyze e (SFC f) = analyze e f
	analyze e Return = e
	analyze e (Returne exp) = analyze e exp
	
instance analyze Exp
where 
	analyze e (I i) = idExists i e
	analyze e (Op2 e1 op e2) = errorsOnly (errorsOnly e e1) e1
	analyze e (Op1 op exp) = errorsOnly e exp
	analyze e (EInt i) = e	
	analyze e (EBrace exp) = analyze e exp
	analyze e (EFC f) = analyze e f
	analyze e EBlock = e
	analyze e (Tup e1 e2) = errorsOnly (errorsOnly e e1) e2
	analyze e bool = e
	
instance analyze FunCall
where analyze e f = foldl (errorsOnly) (idExists f.callName e) f.callArgs
