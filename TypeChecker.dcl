definition module TypeChecker

import DigitalTree, Parser, StdMaybe

:: Env = { subs :: (Type -> Type), functionId :: Maybe Id, freshId :: Int, envLine :: Int, envColumn :: Int }
:: UEnv = { console :: !.File, e :: Env, error :: Bool, global :: .DigitalTree Type, local :: .DigitalTree Type, o1 :: Op1 -> Type, o2 :: Op2 -> Type  }

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

typeFor :: *UEnv !Id -> (Type, *UEnv)
