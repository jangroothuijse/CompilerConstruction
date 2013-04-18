definition module SemanticAnalyzer

import StdMaybe
import Tokenizer
import Parser
import Result

:: Env = { ids :: [(Id, Type)], subs :: (Type -> Type), functionId :: Maybe Id, freshId :: Int, envLine :: Int, envColumn :: Int }
:: UEnv = { console :: !.File, e :: Env, error :: Bool }

check :: Prog *UEnv -> (Prog, *UEnv)

class analyze a :: *UEnv a -> *UEnv
instance analyze Prog
typeFor :: Env !Id -> Type
