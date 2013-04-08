implementation module TypeChecker

// Function in this file do not add to ids to the environment, analyzeType is the only function to do any env modification, and it only makes errors.

import StdEnv
import Parser	// Only Type and Id are really needed
import PrettyPrinter // Only to string Op2 is needed...
import SemanticAnalyzer
import SPLDefaultEnv
import GenEq

returnTypeCheck :: Env RetType RetType -> Env
returnTypeCheck e TVoid TVoid = e
returnTypeCheck e (RT type1) (RT type2) = typeCheck e type1 type2
returnTypeCheck e _ _ = { e & envErrors = ["Unexpecting Void found" :e.envErrors] }

class replaceId a :: Id Type a -> a


instance replaceId Type
where
	replaceId i t (TId j) = if (j == i) t (TId j)
	replaceId i t (TTup (a, b)) = TTup (replaceId i t a, replaceId i t b)
	replaceId i t (TList l) = TList (replaceId i t l)
	replaceId i t (TFun TVoid ta) = TFun TVoid (map (replaceId i t) ta)
	replaceId i t (TFun (RT rt) ta) = TFun (RT (replaceId i t rt)) (map (replaceId i t) ta)
	replaceId i t1 t2 = t2 // TInt etc are unaffected

instance replaceId RetType
where
	replaceId i t TVoid = TVoid
	replaceId i t (RT t2) = RT (replaceId i t t2)

freshId :: Env -> (Env, Id)
freshId e = ({ e & freshId = e.freshId + 1 }, "#" +++ (toString e.freshId))

class allIds a :: a -> [Id]

instance allIds Type where
	allIds (TId i) = [i]
	allIds (TTup (a, b)) = allIds a ++ (allIds b)
	allIds (TList l) = allIds l
	allIds (TFun TVoid ta) = flatten (map (allIds) ta)
	allIds (TFun (RT rt) ta) = allIds rt ++ flatten (map (allIds) ta)
	allIds t = []

instance allIds RetType where
	allIds TVoid = []
	allIds (RT t) = allIds t

//allFresh :: Type Env -> (Env, Type)
//allFresh t e = foldl (\

toFixed :: Type -> Type
toFixed t = foldl (\t2 i -> replaceId i (TFixed i) t2) t (allIds t)

toFixedReturn :: RetType -> RetType
toFixedReturn TVoid = TVoid
toFixedReturn (RT t) = RT (toFixed t)

returnType :: Type -> Type
returnType (TFun rt _) = case rt of 
	TVoid		= TEmpty
	(RT type)	= type
returnType t = t

argTypes :: Type -> [Type]
argTypes (TFun _ args) = args
argTypes t = []

tupleCheck e t = typeCheck { e & envErrors = ["argCheck " +++ (toString (fst t)) +++ " of type " +++ (toString (e.subs (snd t))):e.envErrors]} (fst t) (e.subs (snd t))

isTEmpty TEmpty = True
isTEmpty _ = False

// second type will be the required type
// So typeCheck a b, checks if a meets the requirements of b
instance typeCheck Type where
	typeCheck e TInt TInt = e
	typeCheck e TEmpty TEmpty = e	// Case of void....please dont ask
	typeCheck e TBool TBool = e
	typeCheck e (TTup (a1, a2)) (TTup (b1, b2)) = let x = { (let e2 = (typeCheck e a2 b2) in typeCheck { e2 & subs = e.subs } a1 b1) & subs = e.subs } in
												{ x & envErrors = ["Check tuple " +++ (toString a1) +++ " requiring: " +++ (toString b1) +++
												" AND " +++ (toString a2) +++ " requiring: " +++ (toString b2) :x.envErrors] }
	typeCheck e (TList l1) (TList l2) = typeCheck e l1 l2
	typeCheck e (TFixed i) (TFixed j) =  if (i == j) e { e & envErrors = ["\n!! TYPE ERROR !! " +++ j +++ " does not match " +++ i:e.envErrors] }
	typeCheck e (TId i) (TFixed j) =  { e & subs = (replaceId i (TFixed j)) o e.subs }
	typeCheck e (TFixed i) (TId j) =  { e & subs = (replaceId j (TFixed i)) o e.subs }
//	typeCheck e (TId i) (TId j) = if (i == j) e { e & subs = (replaceId i (TFixed j)) o e.subs }
	typeCheck e found (TId required) = { e & /*envErrors = ["Are you kidding me?" : e.envErrors],*/ subs = (replaceId required found) o e.subs}
	typeCheck e (TId found) requiredType = { e & subs = (replaceId found requiredType) o e.subs }
//  To support higher-order functions, we have no syntax to type higher order expression, but if we did, this would type them:
//	typeCheck e (TFun rt1 tl1) (TFun rt2 tl2) = foldl tupleCheck
//												(if (length tl1 == length tl2) e2
//													{ e2 & envErrors = ["Returntypes don't match or invalid number of arguments":e2.envErrors]})
//												[(x , y) \\ x <- tl1 & y <- tl2]
//												where e2 = returnTypeCheck e rt1 rt2
	typeCheck e a b = { e & envErrors = ["\n!! TYPE ERROR !! " +++ (toString a) +++ " does not match " +++ (toString b):e.envErrors] }

instance typeCheck Exp where
	typeCheck e (I i) type = let vt = (typeFor e i) in (if (isTEmpty vt) { e & envErrors = [i +++ "undefined": e.envErrors] } (typeCheck e vt type))
	typeCheck e (Op2 e1 op e2) type = typeCheck e (EFC { callName = (toString op), callArgs = [e1, e2] }) type
	typeCheck e (Op1 op1 e1) type = typeCheck e (EFC { callName = toId op1, callArgs = [e1] }) type
	where 
		toId PNot = "!"
		toId PNeg = "-u"
	typeCheck e (EInt _) type = typeCheck e TInt type
	typeCheck e (EBrace exp) type = typeCheck e exp type
	typeCheck e EBlock type = typeCheck e (TList (TId "t")) type
	typeCheck e (Tup a1 a2) (TTup (b1, b2)) = { let e2 = (typeCheck e a2 b2) in typeCheck { e2 & subs = e.subs } a1 b1 & subs = e.subs }
	typeCheck e EFalse type = typeCheck e TBool type
	typeCheck e ETrue type = typeCheck e TBool type
	typeCheck e (EFC f) type =  if (isTEmpty funType) { e & envErrors = [f.callName +++ " undefined":e.envErrors] }
								{ foldl tupleCheck e2 [(a, b) \\ a <- f.callArgs & b <- argTypes freshFunType] & subs = e.subs }
	where
		funType = (typeFor e f.callName)
		(e1, freshFunType) = foldl 
						(\et i -> let (e2, fresh) = (freshId (fst et)) in (e2, replaceId i (TId fresh) (snd et)))
						(e, funType)
						(allIds funType)
		e2 = typeCheck { e1 & envErrors = ["Typing " +++ f.callName +++ " requiring type: " +++ (toString freshFunType):e.envErrors]} 
			(returnType freshFunType) type  // e2 may have new restrictions in typeVars

exampleType = (TFun (RT (TList (TId "t"))) [TId "t", TList (TId "t")])
exampleEnv = (typeCheck splDefaultEnv (Op2 ETrue PCons EBlock) (TList TInt))

exampleType2 = TTup (TId "a", TId "b")
exampleExp = Tup ETrue (EInt 5)

Start = typeCheck splDefaultEnv exampleExp exampleType2
//(exampleEnv.envErrors, "\n", (replaceId "t" TBool o id) exampleType, "\n", exampleEnv.subs exampleType)






