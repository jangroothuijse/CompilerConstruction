definition module PrettyPrint

import Tokenizer
import Result
import Parser

prettyPrint :: (Result [Token]) -> String

class pretty a :: Int a -> String
instance pretty Prog
