implementation module TypeChecker

// Function in this file do not add to ids to the environment, analyzeType is the only function to do any env modification, and it only makes errors.

import SPLDefaultEnv
import PrettyPrinter // Only toString Op2, Exp2 and Type is needed...

isVoid :: !RetType -> Bool
isVoid TVoid = True
isVoid (RT _) = False

nonVoid :: !RetType -> Type
nonVoid (RT t) = t

returnTypeCheck :: *UEnv RetType RetType -> *UEnv
returnTypeCheck e t1 t2 = if (not (isVoid t2 || isVoid t2)) (typeCheck e (nonVoid t1) (nonVoid t2)) (if (isVoid t1 && isVoid t2) e (typingError e "Unexpected void"))

instance replaceId Type where
	replaceId i t (TId j) = if (j == i) t (TId j)
	replaceId i t (TTup (a, b)) = TTup (replaceId i t a, replaceId i t b)
	replaceId i t (TList l) = TList (replaceId i t l)
	replaceId i t (TFun TVoid ta) = TFun TVoid (map (replaceId i t) ta)
	replaceId i t (TFun (RT rt) ta) = TFun (RT (replaceId i t rt)) (map (replaceId i t) ta)
	replaceId i t1 t2 = t2 // TInt etc are unaffected
instance replaceId RetType where
	replaceId i t TVoid = TVoid
	replaceId i t (RT t2) = RT (replaceId i t t2)

freshId :: Env -> (Env, Id)
freshId e = ({ e & freshId = e.freshId + 1 }, "#" +++ (toString e.freshId))

instance allIds Type where
	allIds (TId i) = [i]
	allIds (TTup (a, b)) = allIds a ++ (allIds b)
	allIds (TList l) = allIds l
	allIds (TFun TVoid ta) = flatten (map (allIds) ta)
	allIds (TFun (RT rt) ta) = allIds rt ++ flatten (map (allIds) ta)
	allIds t = []
instance allIds RetType where allIds t = if (isVoid t) [] (allIds (nonVoid t))

toFixed :: !a -> a | replaceId, allIds a
toFixed t = foldl (\t2 i -> replaceId i (TFixed i) t2) t (allIds t)

returnType :: !Type -> Type
returnType (TFun rt _) = case rt of 
	TVoid		= TEmpty
	(RT type)	= type
returnType t = t

argTypes :: !Type -> [Type]
argTypes (TFun _ args) = args
argTypes t = []

tupleCheck ue =: { e = e } t = typeCheck ue (fst t) (e.subs (snd t))

isTEmpty TEmpty = True
isTEmpty _ = False

typingError :: *UEnv String -> *UEnv
typingError e =: { e = env } s = { e & console = e.console <<< ("TYPE ERROR: on line " +++ (toString e.e.envLine) +++ " column "
									 +++ (toString e.e.envColumn) +++ " " +++ s +++ "\n"), error = True }

// second type will be the required type, so typeCheck a b, checks if a meets the requirements of b
instance typeCheck Type where
	typeCheck e TInt TInt = e
	typeCheck e TEmpty TEmpty = e	// Case of void....please dont ask
	typeCheck e TBool TBool = e
	typeCheck ue =: { e = e } (TTup (a1, a2)) (TTup (b1, b2)) = { ue3 & e = { ue3.e & subs = e.subs } } where 
		ue2 = (typeCheck ue a2 b2)
		ue3 = typeCheck { ue2 & e = { e & subs = e.subs } } a1 b1
	typeCheck e (TList l1) (TList l2) = typeCheck e l1 l2
	typeCheck e (TFixed i) (TFixed j) =  if (i == j) e (typingError e (j +++ " does not match " +++ i))
	typeCheck ue=:{ e = e } found (TId required) = { ue & e = { e & subs = (replaceId required found) o e.subs } }
	typeCheck ue=:{ e = e } (TId found) requiredType = { ue & e = { e & subs = (replaceId found requiredType) o e.subs } }
//  To support higher-order functions, we have no syntax to type higher order expression, but if we did, this would type them:
//	typeCheck e (TFun rt1 tl1) (TFun rt2 tl2) = foldl tupleCheck
//									(if (length tl1 == length tl2) e2
//									(typingError e "Returntypes don't match or invalid number of arguments")
//									[(x , y) \\ x <- tl1 & y <- tl2]
//									where e2 = returnTypeCheck e rt1 rt2
	typeCheck e a b = (typingError e ((toString a) +++ " does not match " +++ (toString b)))
instance typeCheck Exp where typeCheck ue=:{ e = e } exp t = typeCheck { ue & e = { e & envLine = exp.eline, envColumn = exp.ecolumn } } exp.ex t
instance typeCheck Exp2 where
	typeCheck ue =: { e = e } (I i) type = let vt = (typeFor e i) in (if (isTEmpty vt) (typingError ue (i +++ " undefined")) (typeCheck ue vt type))
	typeCheck e (Op2 e1 op e2) type = typeCheck e (EFC { callName = (toString op), callArgs = [e1, e2] }) type
	typeCheck e (Op1 op1 e1) type = typeCheck e (EFC { callName = toId op1, callArgs = [e1] }) type where 
		toId PNot = "!"
		toId PNeg = "-u"
	typeCheck e (EInt _) type = typeCheck e TInt type
	typeCheck e (EBrace exp) type = typeCheck e exp type
	typeCheck e EBlock type = typeCheck e (TList (TId "t")) type
	typeCheck ue =: { e = e} (Tup a1 a2) (TTup (b1, b2)) = { ue3 & e = { ue3.e & subs = e.subs } } where 
		ue2 = (typeCheck ue a2 b2)
		ue3 = typeCheck { ue2 & e = { e & subs = e.subs } } a1 b1
	typeCheck e EFalse type = typeCheck e TBool type
	typeCheck e ETrue type = typeCheck e TBool type
	typeCheck ue =: { e = e } (EFC f) type
	| isTEmpty funType = (typingError ue (f.callName +++ " undefined"))
	| length f.callArgs <> length aTypes = (typingError ue (f.callName +++ " used with wrong arity"))
	= { ue2 & e = { ue2.e & subs = e.subs } } where
		ue2 = foldl tupleCheck e2 [(a, b) \\ a <- f.callArgs & b <- aTypes]
		aTypes = argTypes freshFunType
		funType = (typeFor e f.callName)
		(env1, freshFunType) = foldl g (e, funType) (allIds funType)
		e1 = { ue & e = env1 }
		e2 = typeCheck e1 (returnType freshFunType) type  // e2 may have new restrictions in typeVars
		g (ue3, t) i = let (ue4, fresh) = (freshId ue3) in (ue4, replaceId i (TId fresh) t)
	typeCheck e p t = typingError e ((toString p) +++ " does not match " +++ (toString t))
