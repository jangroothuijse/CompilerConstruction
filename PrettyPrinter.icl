implementation module PrettyPrinter

import StdEnv, Parser

tabs n = {'\t' \\ i <- [1..n] }
implode glue [] = ""
implode glue [x] = (toString x)
implode glue [x:xs] = (toString x) +++ glue +++ implode glue xs

commaSeperated = implode ", "
notSeperated = implode ""

prettyPrint :: *File Prog -> *File
prettyPrint output decls = foldl (<<<) output [(pretty 0 d) +++ "\n" \\ d <- decls]

instance pretty Decl
where 
	pretty n (V varDecl) = pretty n varDecl
	pretty n (F funDecl) = pretty n funDecl
	pretty n (A algDecl) = (pretty n algDecl) +++ ";\n"
instance pretty VarDecl where pretty n v = (tabs n) +++ (toString v.type) +++ " " +++ (toString v.name) +++ " = " +++ (toString v.exp) +++ ";\n"
instance pretty FunDecl 
where pretty n f = (toString f.retType) +++ " " +++ 
		toString f.funName +++ "(" +++ (commaSeperated f.args) +++ ") {\n"  +++ (notSeperated (map (pretty (n+1)) f.vars))
	 	+++ (notSeperated (map (pretty (n+1)) f.stmts))  +++ "}\n"
instance pretty AlgDecl where pretty n a = (tabs n) +++ "type " +++ (toString a.adname) +++ " " +++ (implode " " a.poly) +++ " = " +++ (implode " | " a.parts)
instance pretty Stmt where 
	pretty n (Block stmts) = (notSeperated (map (pretty (n)) stmts))
	pretty n (If e s) = (tabs n) +++ "if" +++ " (" +++ (toString e) +++ ") {\n" +++ (pretty (n+1) s)  +++ (tabs n) +++ "}\n"
	pretty n (Ife e s1 s2) = (tabs n) +++ "if" +++ " (" +++ (toString e) +++ ") {\n" +++ (pretty (n+1) s1) +++ (tabs n) +++ "} else {\n" +++ (pretty (n+1) s2) +++ (tabs n) +++ "}\n"
	pretty n (While e s) = (tabs n) +++ "while " +++ "(" +++ (toString e) +++ ") {\n" +++ (pretty (n+1) s) +++ (tabs n) +++ "}\n"
	pretty n (Ass i e) = (tabs n) +++ (toString i) +++ " = " +++ (toString e) +++ ";\n"
	pretty n (SFC fun) = (tabs n) +++ (toString fun) +++ ";\n"
	pretty n (Return) = (tabs n) +++ "return" +++ ";\n"
	pretty n (Returne e) = (tabs n) +++ "return " +++ (toString e) +++ ";\n"
	pretty n (Match i cs) = (tabs n) +++ "match (" +++ i +++ ") {\n" +++ ((notSeperated o (map (pretty (n + 1)))) cs) +++ "\n" +++ (tabs n) +++ "}\n"
instance pretty Case where 
	pretty n (Case c [] s) = (tabs n) +++ "(" +++ c +++  ") {\n" +++ (pretty (n+1) s) +++ (tabs n) +++ "}\n"
	pretty n (Case c t s) = (tabs n) +++ "(" +++ c +++ " " +++ (implode " " t) +++ ") {\n" +++ (pretty (n+1) s) +++ (tabs n) +++ "}\n"
instance toString FArg where toString r = (toString r.argType) +++ " " +++ (toString r.argName)
instance toString Type where 
	toString TInt = "Int"
	toString TBool = "Bool"
	toString (TTup (type1, type2)) = "(" +++ (toString type1) +++ ", " +++ (toString type2) +++ ")"
	toString (TList type) = "[" +++ (toString type) +++ "]"
	toString (TId iden) = toString iden
	toString (TFun rt al) = "(" +++ (implode " " al) +++ " -> " +++ (toString rt) +++ ")"
	toString TEmpty = "(No type)"
	toString (TFixed i) = "(Fixed: " +++ (toString i) +++ ")"
	toString (TAlg i []) = i
	toString (TAlg i t) = "(" +++ i +++ " " +++ (implode " " t) +++ ")"
instance toString RetType where
	toString TVoid = "Void"
	toString (RT type) = toString type
instance toString Exp where toString {ex = ex} = toString ex
instance toString Exp2 where
	toString (I iden) = iden
	toString (Op2 e1 op e2) = (toString e1) +++ " " +++ (toString op) +++ " " +++ (toString e2)
	toString (Op1 op e) = (toString op) +++ " " +++ (toString e)
	toString (EInt i) = (toString i)
	toString EFalse = "False"
	toString ETrue = "True"
	toString (EBrace e) = "(" +++ (toString e) +++ ")"
	toString (EFC fun) = toString fun
	toString EBlock = "[]"
	toString (Alg c []) = "new " +++ c
	toString (Alg c [e]) = "new " +++ c +++  " " +++ (toString e)
	toString (Tup e1 e2) = "(" +++ (toString e1) +++ ", " +++ (toString e2) +++ ")"
instance toString FunCall where toString fc = (toString fc.callName) +++ "(" +++ (commaSeperated fc.callArgs) +++ ")"
instance toString Op2 where
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
instance toString Op1 where
	toString PNot = "!"
	toString PNeg = "-"
instance toString AlgPart where toString ap = (toString ap.apname) +++ (if (ap.atype === TEmpty) "" (" " +++ (toString ap.atype)))
derive gEq Type, RetType
