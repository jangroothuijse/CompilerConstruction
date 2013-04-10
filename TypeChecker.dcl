definition module TypeChecker

import Parser
import SemanticAnalyzer

class typeCheck a :: !Env !a !Type -> Env
instance typeCheck Type
instance typeCheck Exp
instance typeCheck Exp2

class returnCheck a :: !a -> Result Bool
instance returnCheck Prog

class replaceId a :: !Id !Type !a -> a
instance replaceId Type
instance replaceId RetType

class allIds a :: !a -> [Id]
instance allIds Type
instance allIds RetType

toFixed :: !a -> a | replaceId, allIds a
returnType :: Type -> Type
