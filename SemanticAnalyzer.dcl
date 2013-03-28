definition module SemanticAnalyzer

import StdEnv
import Tokenizer
import Parser
import Result

:: Env = { ids :: [Id], envErrors :: [String] }

analyzer :: (Prog -> Env)


