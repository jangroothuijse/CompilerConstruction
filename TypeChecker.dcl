definition module TypeChecker

import SemanticAnalyzer

class typeCheck a :: !*UEnv !a !Type -> *UEnv
instance typeCheck Type
instance typeCheck Exp
instance typeCheck Exp2

isVoid :: !RetType -> Bool

class replaceId a :: !Id !Type !a -> a
instance replaceId Type
instance replaceId RetType

class allIds a :: !a -> [Id]
instance allIds Type
instance allIds RetType

toFixed :: !a -> a | replaceId, allIds a
returnType :: !Type -> Type
