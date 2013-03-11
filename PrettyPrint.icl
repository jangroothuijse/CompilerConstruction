implementation module PrettyPrint

import Tokenizer
import Result

prettyPrint :: (Result [TokenOnLine]) -> String
prettyPrint {result = [{token = x}:xs]} = " " +++ (p x) +++ (prettyPrint {result = xs, errors = []})
where
	p :: Token -> String
	p Popen = "("
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
