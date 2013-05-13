implementation module SPLDefaultEnv

import StdEnv, TypeChecker, Parser, AlphaNumIndexed

t :: Type
t = (TId "t")

a :: Type
a = (TId "a")

splDefaultEnv :: !*File -> *UEnv
splDefaultEnv console = { UEnv | console = console, e = { Env | 
	functionId = Nothing, freshId = 0, subs = id, envLine = 0, envColumn = 0 }, error = False, local = newTree 63, o1 = op1Type, o2 = op2Type,
	global = foldl (addIndexed) (newTree 63) [ ("isEmpty", (TFun (RT TBool) [TList t]) ), ("hd", (TFun (RT t) [TList t]) ),
			("tl", (TFun (RT (TList t)) [TList t]) ), ("fst", (TFun (RT a) [TTup (a, t)]) ), 
			("snd", (TFun (RT t) [TTup (a, t)]) ), ("print", (TFun TVoid [t]) ) ] }
			
op2Type :: Op2 -> Type
op2Type PPlus = TFun (RT TInt) [TInt, TInt]
op2Type PMin = TFun (RT TInt) [TInt, TInt]
op2Type PMul = TFun (RT TInt) [TInt, TInt]
op2Type PDiv = TFun (RT TInt) [TInt, TInt]
op2Type PMod = TFun (RT TInt) [TInt, TInt]
op2Type PEq = TFun (RT TBool) [TInt, TInt]
op2Type PLT = TFun (RT TBool) [TInt, TInt]
op2Type PGT = TFun (RT TBool) [TInt, TInt]
op2Type PGTE = TFun (RT TBool) [TInt, TInt]
op2Type PLTE = TFun (RT TBool) [TInt, TInt]
op2Type PNEq = TFun (RT TBool) [TInt, TInt]
op2Type PAnd = TFun (RT TBool) [TBool, TBool]
op2Type POr = TFun (RT TBool) [TBool, TBool]
op2Type PCons = TFun (RT (TList t)) [t, TList t]

op1Type :: Op1 -> Type
op1Type PNot = TFun (RT TBool) [TBool]
op1Type PNeg = TFun (RT TInt) [TInt]
