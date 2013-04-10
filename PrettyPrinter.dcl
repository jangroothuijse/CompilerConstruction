definition module PrettyPrinter

import Tokenizer
import Result
import Parser
import StdEnv

prettyPrint :: (Result [Token]) -> String // Prettyprinter for token streams

class pretty a :: Int a -> String // Pretty printer for AST
instance pretty Prog
instance toString Exp
instance toString Exp2
instance toString Type
instance toString Op2
instance toString Op1
