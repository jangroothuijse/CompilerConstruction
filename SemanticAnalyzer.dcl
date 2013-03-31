definition module SemanticAnalyzer

import StdEnv
import StdMaybe
import Tokenizer
import Parser
import Result

:: Env = { ids :: [(Id, Type)], envErrors :: [String], functionId :: Maybe Id }

class analyze a :: Env a -> Env
instance analyze Prog
typeFor :: Env Id -> Type


