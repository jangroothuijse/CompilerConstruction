definition module SemanticAnalyzer

import StdMaybe, TypeChecker, Parser

check :: *UEnv Prog -> (*UEnv, Prog)

class analyze a :: *UEnv a -> *UEnv
instance analyze Prog
