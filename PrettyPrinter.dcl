definition module PrettyPrinter

import Tokenizer
import Parser
import StdEnv

//prettyPrint :: (Result [Token]) -> String // Prettyprinter for token streams

prettyPrint :: *File Prog -> *File

class pretty a :: Int a -> String // Pretty printer for AST
instance pretty Decl

instance toString Exp
instance toString Exp2
instance toString Type
instance toString RetType
instance toString Op2
instance toString Op1
