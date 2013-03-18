definition module PrettyPrint

import Tokenizer
import Result
import Parser

prettyPrint :: (Result [TokenOnLine]) -> String

class pretty a :: Int a -> String
instance pretty Prog
