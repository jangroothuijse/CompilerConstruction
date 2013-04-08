definition module TypeChecker

import Parser
import SemanticAnalyzer

class typeCheck a :: Env a Type -> Env
instance typeCheck Type
instance typeCheck Exp
instance typeCheck Exp2

class returnCheck a :: a -> Result Bool
instance returnCheck Prog

toFixed :: Type -> Type
toFixedReturn :: RetType -> RetType
returnType :: Type -> Type
