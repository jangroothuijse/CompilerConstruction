implementation module PrettyPrinter

import StdEnv, Parser

tabs n = {'\t' \\ i <- [1..n] }
implode glue [] = ""
implode glue [x] = (toString x)
implode glue [x:xs] = (toString x) +++ glue +++ implode glue xs

AC_RED		:== "\x1b[31m"
AC_GREEN	:== "\x1b[32m"
AC_YELLOW	:== "\x1b[33m"
AC_BLUE		:== "\x1b[34m"
AC_MAGENTA	:== "\x1b[35m"
AC_CYAN		:== "\x1b[36m"
AC_RESET	:== "\x1b[0m"

commaSeperated = implode ", "
notSeperated = implode ""

prettyPrint :: *File Prog -> *File
prettyPrint output decls = foldl (<<<) output [(pretty 0 d) +++ "\n" \\ d <- decls]

instance pretty Decl
where 
	pretty n (V varDecl) = pretty n varDecl
	pretty n (F funDecl) = pretty n funDecl
instance pretty VarDecl where pretty n v = (tabs n) +++ AC_RED +++ (toString v.type) +++ " " +++ AC_CYAN +++ (toString v.name) +++ AC_RESET +++ " = " +++ (toString v.exp) +++ ";\n"
instance pretty FunDecl 
where pretty n f = AC_RED +++ (toString f.retType) +++ AC_RESET +++ " " +++ 
		toString f.funName +++ "(" +++ (commaSeperated f.args) +++ ") {\n"  +++ (notSeperated (map (pretty (n+1)) f.vars))
	 	+++ (notSeperated (map (pretty (n+1)) f.stmts))  +++ "}\n"
instance pretty Stmt where 
	pretty n (Block stmts) = (notSeperated (map (pretty (n)) stmts))
	pretty n (If e s) = (tabs n) +++ AC_MAGENTA +++ "if" +++ AC_RESET +++ " (" +++ (toString e) +++ ") {\n" +++ (pretty (n+1) s)  +++ (tabs n) +++ "}\n"
	pretty n (Ife e s1 s2) = (tabs n) +++ AC_MAGENTA +++ "if" +++ AC_RESET +++ " (" +++ (toString e) +++ ") {\n" +++ (pretty (n+1) s1) +++ (tabs n) +++ "} else {\n" +++ (pretty (n+1) s2) +++ (tabs n) +++ "}\n"
	pretty n (While e s) = (tabs n) +++ AC_MAGENTA +++ "while " +++ AC_RESET +++ "(" +++ (toString e) +++ ") {\n" +++ (pretty (n+1) s) +++ (tabs n) +++ "}\n"
	pretty n (Ass i e) = (tabs n) +++ AC_CYAN +++ (toString i) +++ AC_RESET +++ " = " +++ (toString e) +++ ";\n"
	pretty n (SFC fun) = (tabs n) +++ (toString fun) +++ ";\n"
	pretty n (Return) = (tabs n) +++ AC_MAGENTA +++ "return" +++ AC_RESET +++ ";\n"
	pretty n (Returne e) = (tabs n) +++ AC_MAGENTA +++ "return " +++ AC_RESET +++ (toString e) +++ ";\n"
instance toString FArg where toString r = AC_RED +++ (toString r.argType) +++ " " +++ AC_CYAN +++ (toString r.argName) +++ AC_RESET
instance toString Type where 
	toString TInt = "Int"
	toString TBool = "Bool"
	toString (TTup (type1, type2)) = "(" +++ (toString type1) +++ ", " +++ (toString type2) +++ ")"
	toString (TList type) = "[" +++ (toString type) +++ "]"
	toString (TId iden) = AC_YELLOW +++ toString iden +++ AC_RED
	toString (TFun rt al) = "(" +++ (implode " " al) +++ " -> " +++ (toString rt) +++ ")"
	toString TEmpty = "(No type)"
	toString (TFixed i) = "(Fixed: " +++ (toString i) +++ ")"
instance toString RetType where
	toString TVoid = "Void"
	toString (RT type) = toString type
instance toString Exp where toString {ex = ex} = toString ex
instance toString Exp2 where
	toString (I iden) = AC_CYAN +++ iden +++ AC_RESET
	toString (Op2 e1 op e2) = (toString e1) +++ " " +++ (toString op) +++ " " +++ (toString e2)
	toString (Op1 op e) = (toString op) +++ " " +++ (toString e)
	toString (EInt i) = AC_BLUE +++ (toString i) +++ AC_RESET
	toString EFalse = AC_BLUE +++ "False" +++ AC_RESET
	toString ETrue = AC_BLUE +++ "True" +++ AC_RESET
	toString (EBrace e) = "(" +++ (toString e) +++ ")"
	toString (EFC fun) = toString fun
	toString EBlock = "[]"
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
