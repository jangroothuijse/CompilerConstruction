definition module PrettyPrinter

import Tokenizer
import Result
import Parser
import StdEnv

prettyPrint :: (Result [Token]) -> String

class pretty a :: Int a -> String
instance pretty Prog

instance toString Exp
instance toString Type
instance toString Op2
instance toString Op1
