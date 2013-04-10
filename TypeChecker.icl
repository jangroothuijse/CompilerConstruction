implementation module TypeChecker

// Function in this file do not add to ids to the environment, analyzeType is the only function to do any env modification, and it only makes errors.

import StdEnv
import Parser	// Only Type and Id are really needed
import PrettyPrinter // Only to string Op2 is needed...
import SemanticAnalyzer
import SPLDefaultEnv
import GenEq

isVoid :: RetType -> Bool
isVoid TVoid = True
isVoid _ = False

nonVoid :: RetType -> Type
nonVoid (RT t) = t

returnTypeCheck :: Env RetType RetType -> Env
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

freshId :: !Env -> (Env, Id)
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

returnType :: Type -> Type
returnType (TFun rt _) = case rt of 
	TVoid		= TEmpty
	(RT type)	= type
returnType t = t

argTypes :: Type -> [Type]
argTypes (TFun _ args) = args
argTypes t = []

tupleCheck e t = typeCheck e (fst t) (e.subs (snd t))

isTEmpty TEmpty = True
isTEmpty _ = False

typingError e s = { e & envErrors = ["TYPING ERROR on line " +++ (toString e.envLine) +++ " column " +++ (toString e.envColumn) +++ ": " +++ s : e.envErrors] }

// second type will be the required type
// So typeCheck a b, checks if a meets the requirements of b
instance typeCheck Type where
	typeCheck e TInt TInt = e
	typeCheck e TEmpty TEmpty = e	// Case of void....please dont ask
	typeCheck e TBool TBool = e
	typeCheck e (TTup (a1, a2)) (TTup (b1, b2)) = { (let e2 = (typeCheck e a2 b2) in typeCheck { e2 & subs = e.subs } a1 b1) & subs = e.subs }
	typeCheck e (TList l1) (TList l2) = typeCheck e l1 l2
	typeCheck e (TFixed i) (TFixed j) =  if (i == j) e (typingError e (j +++ " does not match " +++ i))
	typeCheck e found (TId required) = { e & subs = (replaceId required found) o e.subs}
	typeCheck e (TId found) requiredType = { e & subs = (replaceId found requiredType) o e.subs }
//  To support higher-order functions, we have no syntax to type higher order expression, but if we did, this would type them:
//	typeCheck e (TFun rt1 tl1) (TFun rt2 tl2) = foldl tupleCheck
//									(if (length tl1 == length tl2) e2
//									(typingError e "Returntypes don't match or invalid number of arguments")
//									[(x , y) \\ x <- tl1 & y <- tl2]
//									where e2 = returnTypeCheck e rt1 rt2
	typeCheck e a b = (typingError e ((toString a) +++ " does not match " +++ (toString b)))

instance typeCheck Exp where typeCheck e exp t = typeCheck { e & envLine = exp.eline, envColumn = exp.ecolumn } exp.ex t

instance typeCheck Exp2 where
	typeCheck e (I i) type = let vt = (typeFor e i) in (if (isTEmpty vt) (typingError e (i +++ "undefined")) (typeCheck e vt type))
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
	typeCheck e (EFC f) type =  if (isTEmpty funType) (typingError e (f.callName +++ " undefined"))
									if (length f.callArgs <> length aTypes)
									(typingError e (f.callName +++ " used with wrong arity"))
									{ foldl tupleCheck e2 [(a, b) \\ a <- f.callArgs & b <- aTypes] & subs = e.subs }
	where
		aTypes = argTypes freshFunType
		funType = (typeFor e f.callName)
		(e1, freshFunType) = foldl 
						(\et i -> let (e2, fresh) = (freshId (fst et)) in (e2, replaceId i (TId fresh) (snd et)))
						(e, funType)
						(allIds funType)
		e2 = typeCheck e1 (returnType freshFunType) type  // e2 may have new restrictions in typeVars

returnError e s = { e & envErrors = [s +++ (if (isJust e.functionId) (" in function " +++ (fromJust e.functionId)) ""):e.envErrors] }

returnCheck :: !Env !FunDecl -> Env
returnCheck e f = let (g, b) = foldl (rtCheck) (e, False) f.stmts in if (b || (isVoid f.retType)) g (returnError g "Not all branches have a return")
where
	rtCheck :: !(!Env, !Bool) Stmt -> (Env, Bool)
	rtCheck (e, True) _ 		= (returnError e "Unreachable code found (statements after return)", True)
	rtCheck t (Block stmts) 	= foldl (rtCheck) t stmts
	rtCheck t (If _ stmt) 		= (fst (rtCheck t stmt), False) // An if cannot definitively return, but can throw unreachale errors
	rtCheck t (Ife _ s1 s2) 	= let (e, b1) = (rtCheck t s1) in (let (e2, b2) = (rtCheck (e, False) s2) in (e2, b1 && b2))
	rtCheck t (While _ stmt) 	= (fst (rtCheck t stmt), False) // See if..
	rtCheck (e, _) Return 		= (e, True)
	rtCheck (e, _) (Returne _) 	= (e, True)
	rtCheck t _ 				= t // Assignment, functioncalls
