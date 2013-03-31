definition module TypeChecker

analyzeType :: Env a Type -> Env | typeCheck, toString a

import Parser
import SemanticAnalyzer

class typeCheck a :: Env a Type -> Bool
instance typeCheck Type
instance typeCheck Exp

returnType :: Type -> Type
