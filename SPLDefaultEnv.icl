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
			
			("==", (TFun (RT TBool) [TInt, TInt]) ),
			("<", (TFun (RT TBool) [TInt, TInt]) ),
			(">", (TFun (RT TBool) [TInt, TInt]) ),
			("<=", (TFun (RT TBool) [TInt, TInt]) ),
			(">=", (TFun (RT TBool) [TInt, TInt]) ),
			("!=", (TFun (RT TBool) [TInt, TInt]) ),
			("!", (TFun (RT TBool) [TBool]) ),
			
			("&&", (TFun (RT TBool) [TBool, TBool]) ),
			("||", (TFun (RT TBool) [TBool, TBool]) ),
			
			(":", (TFun (RT (TList t)) [t, TList t]) ),
			("isEmpty", (TFun (RT TBool) [TList t]) ),
			("hd", (TFun (RT t) [TList t]) ),
			("tl", (TFun (RT (TList t)) [TList t]) ),
			
			("fst", (TFun (RT a) [TTup (a, t)]) ),
			("snd", (TFun (RT t) [TTup (a, t)]) ),
			
			("print", (TFun TVoid [t]) )		
			],
	envErrors = [], functionId = Nothing, freshId = 0, subs = id, envLine = 0, envColumn = 0 }
