implementation module TypeChecker

// Function in this file do not add to ids to the environment, analyzeType is the only function to do any env modification, and it only makes errors.

import StdEnv
import Parser	// Only Type and Id are really needed
import PrettyPrinter // Only to string Op2 is needed...
import SemanticAnalyzer
import SPLDefaultEnv

class typeCheck a :: Env a Type -> Bool

replaceId :: Id Type Type -> Type
replaceId i t (TId j) = if (j == i) t (TId j)
replaceId i t (TTup (a, b)) = TTup (replaceId i t a, replaceId i t b)
replaceId i t (TList l) = TList (replaceId i t l)
replaceId i t (TFun TVoid ta) = TFun TVoid (map (replaceId i t) ta)
replaceId i t (TFun (RT rt) ta) = TFun (RT (replaceId i t rt)) (map (replaceId i t) ta)
replaceId i t1 t2 = t2 // TInt etc are unaffected

// :: foundType requiredType -> replaceFunctions
coerceType :: Env Type Type -> (Bool, (Type -> Type))
coerceType e (TId i) t = (True, replaceId i t)
coerceType e (TTup (a1, a2)) (TTup (b1, b2))
# (succes, t1) 	= (coerceType e a1 b1)
| not succes		= (False, id)
# (succes, t2) 	= (coerceType e a2 b2)
| not succes		= (False, id)
				= (True, t1 o t2)
coerceType e (TList t1) (TList t2) = coerceType e t1 t2
coerceType e givenType requiredType = (typeCheck e givenType requiredType, id) // TInt etc will not need coersions
// We are not supporting higher order functions yet, whe the time come, TFunc alternatives will have to be made

returnTypeCheck :: Env RetType RetType -> Bool
returnTypeCheck e TVoid TVoid = True
returnTypeCheck e (RT type1) (RT type2) = typeCheck e type1 type2
returnTypeCheck e _ _ = False

// Creates new fresh type ids for any type id we might encounter
typeOf :: Env Exp -> (Env, Type)
typeOf e (I i) = (e, typeFor e i)
typeOf e (Op2 e1 op e2) = typeOf e (EFC { callName = (toString op), callArgs = [e1, e2] })
typeOf e (Op1 op1 e1) = typeOf e (EFC { callName = toId op1, callArgs = [e1] })
where 
	toId PNot = "!"
	toId PNeg = "-u"
typeOf e (EInt _) = (e, TInt)
typeOf e (EBrace exp) = typeOf e exp
typeOf e EBlock = ({ e & freshId = e.freshId + 1 }, TList (TId ("#" +++ toString e.freshId)))
typeOf e (Tup e1 e2)
# (e, t1) = typeOf e e1
# (e, t2) = typeOf e e2
= (e, TTup (t1, t2))
typeOf e EFalse = (e, TBool)
typeOf e ETrue = (e, TBool)
// For function call, we need to replace all type Id's by fresh one's, but a -> a must ofc become #1 -> #1
typeOf e (EFC f) = replaceListOfIds funType (listOfIds [] funType) e
where 
	funType = (typeFor e f.callName)
	listOfIds :: [Id] Type -> [Id]
	listOfIds acc (TTup (t1, t2)) = listOfIds (listOfIds acc t2) t1 
	listOfIds acc (TList t) = listOfIds acc t
	listOfIds acc (TFun (RT rt) l) = foldl (listOfIds) (listOfIds acc rt) l
	listOfIds acc (TFun TVoid l) = foldl (listOfIds) acc l
	listOfIds acc (TId i) = [i:acc]
	listOfIds acc _ = acc
	replaceListOfIds :: Type [Id] Env -> (Env, Type)
	replaceListOfIds t [] e = (e, t)
	replaceListOfIds t [i:xs] e = replaceListOfIds (replaceId i t (TId ("#" +++ toString e.freshId))) xs { e & freshId = e.freshId + 1 }

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
	typeCheck e _ (TId i) = True				// If a generic type is required, a more specific type may be used
	typeCheck e (TId i) _  = True				// If a generic type is required, a more specific type may be used	
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
	typeCheck e (EFC f) type = r
	where
		(r, _, _) = foldl (g) (returnSucces, e, coersions) [(a, b) \\ a <- f.callArgs, b <- argTypes]
		funType = typeFor e f.callName
		argTypes = case funType of
			TFun rt args 	= args
			_				= []
		returnType2 = returnType funType
		(returnSucces, coersions) = coerceType e returnType2 type
		g :: (Bool, Env, Type -> Type) (Exp, Type) -> (Bool, Env, Type -> Type)
		g (False, env, c) _ = (False, env, c)
		g (_, env, c) (exp, requiredType)
		# (env, t)	= (typeOf env exp)
		# (succes, c) = coerceType env t (c requiredType)
		= (succes, env, c)
		
	typeCheck e _ (TId _) = True
	typeCheck e _ _ = False
	
analyzeType :: Env a Type -> Env | typeCheck, toString a
analyzeType env t1 t2 = if (typeCheck env t1 t2) env 
						{ env & envErrors = [("Type required " +++ (toString t2) +++ " but found " +++ (toString t1)):env.envErrors] }

Start = (analyzeType { ids = [], envErrors = [], functionId = Nothing, freshId = 0 } (TFun TVoid [TBool, TInt]) (TFun TVoid [TBool, TBool])).envErrors
