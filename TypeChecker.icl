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

tupleCheck e t = typeCheck e (fst t) (e.subs (snd t))

isTEmpty TEmpty = True
isTEmpty _ = False

typingError e = "TYPING ERROR on line " +++ (toString e.envLine) +++ " column " +++ (toString e.envColumn) +++ ": "

// second type will be the required type
// So typeCheck a b, checks if a meets the requirements of b
instance typeCheck Type where
	typeCheck e TInt TInt = e
	typeCheck e TEmpty TEmpty = e	// Case of void....please dont ask
	typeCheck e TBool TBool = e
	typeCheck e (TTup (a1, a2)) (TTup (b1, b2)) = { (let e2 = (typeCheck e a2 b2) in typeCheck { e2 & subs = e.subs } a1 b1) & subs = e.subs }
	typeCheck e (TList l1) (TList l2) = typeCheck e l1 l2
	typeCheck e (TFixed i) (TFixed j) =  if (i == j) e { e & envErrors = [typingError e +++ j +++ " does not match " +++ i:e.envErrors] }
	typeCheck e found (TId required) = { e & subs = (replaceId required found) o e.subs}
	typeCheck e (TId found) requiredType = { e & subs = (replaceId found requiredType) o e.subs }
//  To support higher-order functions, we have no syntax to type higher order expression, but if we did, this would type them:
//	typeCheck e (TFun rt1 tl1) (TFun rt2 tl2) = foldl tupleCheck
//									(if (length tl1 == length tl2) e2
//									{ e2 & envErrors = [typingError e +++ "Returntypes don't match or invalid number of arguments":e2.envErrors]})
//									[(x , y) \\ x <- tl1 & y <- tl2]
//									where e2 = returnTypeCheck e rt1 rt2
	typeCheck e a b = { e & envErrors = [typingError e +++ (toString a) +++ " does not match " +++ (toString b):e.envErrors] }

instance typeCheck Exp where
	typeCheck e {ex = ex} t = typeCheck e ex t

instance typeCheck Exp2 where
	typeCheck e (I i) type = let vt = (typeFor e i) in (if (isTEmpty vt) { e & envErrors = [typingError e +++ i +++ "undefined": e.envErrors] } (typeCheck e vt type))
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
	typeCheck e (EFC f) type =  if (isTEmpty funType) { e & envErrors = [typingError e +++ f.callName +++ " undefined":e.envErrors] }
								if (length f.callArgs <> length aTypes)
								{ e & envErrors = [typingError e +++ f.callName +++ " used with wrong arity":e.envErrors] }
								{ foldl tupleCheck e2 [(a, b) \\ a <- f.callArgs & b <- aTypes] & subs = e.subs }
	where
		aTypes = argTypes freshFunType
		funType = (typeFor e f.callName)
		(e1, freshFunType) = foldl 
						(\et i -> let (e2, fresh) = (freshId (fst et)) in (e2, replaceId i (TId fresh) (snd et)))
						(e, funType)
						(allIds funType)
		e2 = typeCheck e1 (returnType freshFunType) type  // e2 may have new restrictions in typeVars

class returnCheck a :: a -> Result Bool

instance returnCheck Prog where
	returnCheck [x:xs] = case returnCheck x of
								Res _ = returnCheck xs
								Err e    = case returnCheck xs of
												Res _ = Err e
												Err es = Err (e ++ es)
	returnCheck [] = Res True

instance returnCheck Decl where
	returnCheck (F {funName = n, retType = RT _, stmts = stmts}) = case returnChecks stmts of
		Res False = Err ["Function " +++ n +++ " doesn't return."]
		other     = other
	returnCheck (F {stmts = stmts}) = returnChecks stmts
	returnCheck _ = Res True

instance returnCheck Stmt where
	returnCheck (Block stmts) = returnChecks stmts
	returnCheck (If _ stmt) = returnCheck stmt
	returnCheck (Ife _ stmt stmt2) = case returnCheck stmt of
		Res True = returnCheck stmt2
		Res False = case returnCheck stmt2 of
						Res _ = Res False
						Err es = Err es
		Err e    = case returnCheck stmt2 of
						Res _ = Err e
						Err es = Err (e ++ es)
	returnCheck (While _ stmts) = returnCheck stmts
	returnCheck (Ass _ _) = Res False
	returnCheck (SFC _) = Res False
	returnCheck (Return) = Res True
	returnCheck (Returne _) = Res True

returnChecks :: [Stmt] -> Result Bool
returnChecks [x:[]] = returnCheck x
returnChecks [x:xs] = case returnCheck x of
							Res True = Err ["Unreachable code"]
							Res False = returnChecks xs
							other = other
returnChecks [] = Res False


exampleType = (TFun (RT (TList (TId "t"))) [TId "t", TList (TId "t")])
exampleEnv = (typeCheck splDefaultEnv ({ex = Op2 {ex = ETrue, eline = 0, ecolumn = 0} PCons {ex = EBlock, eline = 0, ecolumn = 0}, eline = 0, ecolumn = 0}) (TList TInt))

exampleType2 = TTup (TId "a", TId "b")
exampleExp = {ex = Tup {ex = ETrue, eline = 0, ecolumn = 0} {ex = EInt 5, eline = 0, ecolumn = 0}, eline = 0, ecolumn = 0}

Start = typeCheck splDefaultEnv exampleExp exampleType2
//(exampleEnv.envErrors, "\n", (replaceId "t" TBool o id) exampleType, "\n", exampleEnv.subs exampleType)
