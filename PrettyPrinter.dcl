definition module PrettyPrinter

import Parser, StdEnv

prettyPrint :: *File Prog -> *File

class pretty a :: Int a -> String // Pretty printer for AST
instance pretty Decl

instance toString Exp
instance toString Exp2
instance toString Type
instance toString RetType
instance toString Op2
instance toString Op1
