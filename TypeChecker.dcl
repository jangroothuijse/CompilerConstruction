definition module TypeChecker

import Parser
import SemanticAnalyzer

class typeCheck a :: Env a Type -> Env
instance typeCheck Type
instance typeCheck Exp

toFixed :: Type -> Type
toFixedReturn :: RetType -> RetType
returnType :: Type -> Type
