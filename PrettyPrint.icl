implementation module PrettyPrint

import StdEnv
import Tokenizer
import Parser
import Result

prettyPrint :: (Result [Token]) -> String
prettyPrint {result = [{token = x}:xs]} = " " +++ (p x) +++ (prettyPrint {result = xs, errors = []})
where
	p :: Symbol -> String
	p POpen = "("
	p PClose = ")"
	p CBOpen = "{\n"
	p CBClose = "\n}\n"
	p SBOpen = "["
	p SBClose = "]"
	p Comma = ","
	p Semicolon = ";\n"
	p (Identifier s) = s
	p (Integer x) = toString x
	p (Op Plus) = "+"
	p (Op Min) = "-"
	p (Op Mul) = "*"
	p (Op Div) = "/"
	p (Op Mod) = "%"
	p (Op Eq) = "=="
	p (Op LT) = "<"
	p (Op GT) = ">"
	p (Op LTE) = "<="
	p (Op GTE) = ">="
	p (Op NEq) = "!="
	p (Op And) = "&&"
	p (Op Or) = "||"
	p (Op Cons) = ":"
	p (Op Not) = "!"
	p (Op _) = abort "unknown operator"
	p KIf = "if"
	p KElse = "else" 
	p KWhile = "while"
	p KReturn = "return"
	p KVoid  = "void"
	p KInt = "int"
	p KBool = "bool"
	p KTrue = "true"
	p KFalse = "false"
	p KAssign = "="
	p _ = abort "unknown token"
prettyPrint _ = ""

tabs n = {'\t' \\ i <- [1..n] }
implode glue [] = ""
implode glue [x] = (toString x)
implode glue [x:xs] = (toString x) +++ glue +++ implode glue xs

commaSeperated = implode ", "
notSeperated = implode ""

class pretty a :: Int a -> String

instance pretty Prog
where pretty _ decls = foldl (+++) "" [(pretty 0 d) +++ "\n" \\ d <- decls]

instance pretty Decl
where 
	pretty n (V varDecl) = pretty n varDecl
	pretty n (F funDecl) = pretty n funDecl
	
instance pretty VarDecl
where pretty n v = (tabs n) +++ (toString v.type) +++ " " +++ (toString v.name) +++ " = " +++ (toString v.exp) +++ ";\n"

instance pretty FunDecl
where pretty n f = (toString f.retType) +++ " " +++ 
		toString f.funName +++ "(" +++ (commaSeperated f.args) +++ ") {\n"
		 +++ (notSeperated (map (pretty (n+1)) f.vars))
		 +++ (notSeperated (map (pretty (n+1)) f.stmts))
		 +++ "}\n"
		 		 
instance pretty Stmt
where 
	pretty n (Block stmts) = (notSeperated (map (pretty (n)) stmts))
	pretty n (If e s) = (tabs n) +++ "if (" +++ (toString e) +++ ") {\n" +++ (pretty (n+1) s)  +++ (tabs n) +++ "}\n"
	pretty n (Ife e s1 s2) = (tabs n) +++ "if (" +++ (toString e) +++ ") {\n" +++ (pretty (n+1) s1) +++ (tabs n) +++ "} else {\n" +++ (pretty (n+1) s2) +++ (tabs n) +++ "}\n"
	pretty n (While e s) = (tabs n) +++ "while (" +++ (toString e) +++ ") {\n" +++ (pretty (n+1) s) +++ (tabs n) +++ "}\n"
	pretty n (Ass i e) = (tabs n) +++ (toString i) +++ " = " +++ (toString e) +++ ";\n"
	pretty n (SFC fun) = (tabs n) +++ (toString fun) +++ ";\n"
	pretty n (Return) = (tabs n) +++ "return;\n"
	pretty n (Returne e) = (tabs n) +++ "return " +++ (toString e) +++ ";\n"

instance toString FArg
where toString r = (toString r.argType) +++ " " +++ (toString r.argName)

instance toString Type
where 
	toString TInt = "Int"
	toString PBool = "Bool"
	toString (TTup (type1, type2)) = "(" +++ (toString type1) +++ ", " +++ (toString type2) +++ ")"
	toString (TList type) = "[" +++ (toString type) +++ "]"
	toString (TId iden) = toString iden

instance toString RetType
where
	toString (RT type) = toString type
	toString PVoid = "Void"
	
instance toString Exp
where
	toString (I iden) = toString iden
	toString (Op2 e1 op e2) = (toString e1) +++ " " +++ (toString op) +++ " " +++ (toString e2)
	toString (Op1 op e) = (toString op) +++ " " +++ (toString e)
	toString (EInt i) = (toString i)
	toString EFalse = "False"
	toString ETrue = "True"
	toString (EBrace e) = "(" +++ (toString e) +++ ")"
	toString (EFC fun) = toString fun
	toString EBlock = "[]"
	toString (Tup e1 e2) = "(" +++ (toString e1) +++ ", " +++ (toString e2) +++ ")"

instance toString FunCall
where
	toString fc = (toString fc.callName) +++ "(" +++ (commaSeperated fc.callArgs) +++ ")"
	
instance toString Op2
where
	toString PPlus = "+"
	toString PMin = "-"
	toString PMul = "*"
	toString PDiv = "/"
	toString PMod = "%"
	toString PEq = "=="
	toString PLT = "<"
	toString PGT = ">"
	toString PLTE = "<="
	toString PGTE = ">="
	toString PNEq = "!="
	toString PAnd = "&&"
	toString POr = "||"
	toString PCons = ":"
	
instance toString Op1
where
	toString PNot = "!"
	toString PNeg = "-"
	
