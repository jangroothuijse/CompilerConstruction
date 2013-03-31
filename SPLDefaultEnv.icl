implementation module SPLDefaultEnv

import StdEnv
import Parser
import PrettyPrinter
import SemanticAnalyzer

t :: Type
t = (TId "t")

a :: Type
a = (TId "a")

splDefaultEnv :: Env
splDefaultEnv = { ids = [
			("+", (TFun (RT TInt) [TInt, TInt]) ),
			("-", (TFun (RT TInt) [TInt, TInt]) ),
			("*", (TFun (RT TInt) [TInt, TInt]) ),
			("/", (TFun (RT TInt) [TInt, TInt]) ),
			("%", (TFun (RT TInt) [TInt, TInt]) ),
			("-u", (TFun (RT TInt) [TInt]) ),
			
			("==", (TFun (RT PBool) [TInt, TInt]) ),
			("<", (TFun (RT PBool) [TInt, TInt]) ),
			(">", (TFun (RT PBool) [TInt, TInt]) ),
			("<=", (TFun (RT PBool) [TInt, TInt]) ),
			(">=", (TFun (RT PBool) [TInt, TInt]) ),
			("!=", (TFun (RT PBool) [TInt, TInt]) ),
			("!", (TFun (RT PBool) [PBool]) ),
			
			("&&", (TFun (RT PBool) [PBool, PBool]) ),
			("||", (TFun (RT PBool) [PBool, PBool]) ),
			
			(":", (TFun (RT t) [t, TList t]) ),
			("isEmpty", (TFun (RT PBool) [TList t]) ),
			("hd", (TFun (RT t) [TList t]) ),
			("tl", (TFun (RT (TList t)) [TList t]) ),
			
			("fst", (TFun (RT a) [TTup (a, t)]) ),
			("snd", (TFun (RT t) [TTup (a, t)]) ),
			
			("print", (TFun PVoid [t]) )		
			],
	envErrors = [], functionId = Nothing }
