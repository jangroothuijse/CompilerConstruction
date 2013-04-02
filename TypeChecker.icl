implementation module TypeChecker

// Function in this file do not add to ids to the environment, analyzeType is the only function to do any env modification, and it only makes errors.

import StdEnv
import Parser	// Only Type and Id are really needed
import PrettyPrinter // Only to string Op2 is needed...
import SemanticAnalyzer

class typeCheck a :: Env a Type -> Bool

returnTypeCheck :: Env RetType RetType -> Bool
returnTypeCheck e TVoid TVoid = True
returnTypeCheck e (RT type1) (RT type2) = typeCheck e type1 type2
returnTypeCheck e _ _ = False

returnType :: Type -> Type
returnType (TFun rt _) = case rt of 
	TVoid		= TEmpty
	(RT type)	= type
returnType t = t

// second type will be the required type
// So typeCheck a b, checks if a meets the requirements of b
instance typeCheck Type where
	typeCheck e TInt TInt = True
	typeCheck e TEmpty TEmpty = True	// Case of void....please dont ask
	typeCheck e TBool TBool = True
	typeCheck e (TTup (a1, a2)) (TTup (b1, b2)) = (typeCheck e a1 b1) && (typeCheck e a2 b2)
	typeCheck e (TList l1) (TList l2) = typeCheck e l1 l2
	typeCheck e (TId i1) _ = True				// So that generic operations can be defined on other generic operations
	typeCheck e (TFun rt1 tl1) (TFun rt2 tl2) = foldl (&&) (returnTypeCheck e rt1 rt2 && length tl1 == length tl2) (map (\z . typeCheck e (fst z) (snd z)) [(x,y)\\x<-tl1&y<-tl2])
	typeCheck e a b = False		// No implicit casting, not even function without arguments to their return value

// So if a is required, TBool will do
// But if TBool is required, a will not do
// Hence we have 'typeCheck e (TId i1) _ = True' but not 'typeCheck e _ (TId i1) = True'

instance typeCheck Exp where
	typeCheck e (I i) type = typeCheck e (typeFor e i) type
	typeCheck e (Op2 e1 op e2) type = typeCheck e (EFC { callName = (toString op), callArgs = [e1, e2] }) type
	typeCheck e (Op1 op1 e1) type = typeCheck e (EFC { callName = toId op1, callArgs = [e1] }) type
	where 
		toId PNot = "!"
		toId PNeg = "-u"
	typeCheck e (EInt _) type = typeCheck e TInt type
	typeCheck e (EBrace exp) type = typeCheck e exp type
	typeCheck e EBlock type = typeCheck e (TList (TId "t")) type
	typeCheck e (Tup e1 e2) (TTup (b1, b2)) = typeCheck e e1 b1 && typeCheck e e2 b2
	typeCheck e EFalse type = typeCheck e TBool type
	typeCheck e ETrue type = typeCheck e TBool type
//	typeCheck e (EFC f) (TFun rt2 tl2) = foldr (&&) (returnTypeCheck e ft rt2 && length f.callArgs == length tl2) (map (\t . typeCheck e (fst t) (snd t)) [(x,y)\\x <- f.callArgs & y <- tl2])
//	where 
//		ft = RT (typeFor e f.callName) // Perhaps a correct rule for Id's when we support higher order functions...?
	typeCheck e (EFC f) type = typeCheck e (returnType (typeFor e f.callName)) type
	typeCheck e _ (TId _) = True
	typeCheck e _ _ = False
	
analyzeType :: Env a Type -> Env | typeCheck, toString a
analyzeType env t1 t2 = if (typeCheck env t1 t2) env 
						{ env & envErrors = [("Type required " +++ (toString t2) +++ " but found " +++ (toString t1)):env.envErrors] }

Start = (analyzeType { ids = [], envErrors = [], functionId = Nothing } (TFun TVoid [TBool, TInt]) (TFun TVoid [TBool, TBool])).envErrors
