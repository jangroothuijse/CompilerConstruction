implementation module TypeChecker

// Function in this file do not add to ids to the environment, analyzeType is the only function to do any env modification, and it only makes errors.

import StdEnv, SPLDefaultEnv, AlphaNumIndexed, PrettyPrinter

isVoid :: !RetType -> Bool
isVoid TVoid = True
isVoid (RT _) = False

nonVoid :: !RetType -> Type
nonVoid (RT t) = t

Start = foldl (+++) "" (map (toNumbers) ["id0", "x", "id1000", "1023", "aB", "zZ", "_a"])

toNumbers :: String -> String
toNumbers s = foldl (+++) "\n" [ (toString o toInt) c \\ c <-: s ]

typeFor :: *UEnv !Id -> (Type, *UEnv)
typeFor e=:{ local = local, global = global } i
	# (result, local) = local getIndexed (i, TEmpty)
	| not (result === TEmpty) = (result, { e & local = local })
	# (result, global) = global getIndexed (i, TEmpty)
	| not (result === TEmpty) = (result, { e & local = local, global = global })
	# (e, result) = getro { e & local = local, global = global } i
	| result === TEmpty = abort ("Cannot find " +++ i)
	= (result, e)
	

returnTypeCheck :: *UEnv RetType RetType -> *UEnv
returnTypeCheck e t1 t2 = if (not (isVoid t2 || isVoid t2)) (typeCheck e (nonVoid t1) (nonVoid t2)) (if (isVoid t1 && isVoid t2) e (typingError e "Unexpected void"))

instance replaceId Type where
	replaceId i t (TId j) = if (j == i) t (TId j)
	replaceId i t (TTup (a, b)) = TTup (replaceId i t a, replaceId i t b)
	replaceId i t (TList l) = TList (replaceId i t l)
	replaceId i t (TFun TVoid ta) = TFun TVoid (map (replaceId i t) ta)
	replaceId i t (TFun (RT rt) ta) = TFun (RT (replaceId i t rt)) (map (replaceId i t) ta)
	replaceId i t (TAlg n ta) = TAlg n (map (replaceId i t) ta)
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
	allIds (TAlg n ta) = flatten (map (allIds) ta)
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

algCT :: AlgDecl Id -> Maybe Type
algCT { parts = parts } c = f parts where
	f [] = Nothing
	f [{apname = apname, atype = atype}:xs] = if (apname == c) (Just atype) (f xs)
	

algRewrite :: AlgDecl [Type] -> (Type -> Type)
algRewrite { poly = poly } params = foldl (\f (x, y) -> (f o (replaceId x y))) id [(x, y) \\ x <- poly & y <- params]

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
	typeCheck e (TAlg n1 p1) (TAlg n2 p2) = if (n1 == n2) (foldl tupleCheck e [(x,y)\\x<-p1&y<-p2]) (typingError e (n1 +++ " does not match " +++ n2))
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
	typeCheck ue (I i) type
		# (vt, ue) = typeFor ue i
		| isTEmpty vt = typingError ue (i +++ "undefined")
		= typeCheck ue vt type
	typeCheck e=:{ e = env } (Op2 e1 op e2) type
	# (env, opType) = let type = e.o2 op in freeIds (env, type) (allIds type) // refresh type vars
	# e = typeCheck { e & e = env } (returnType opType) type
	= typeCheck (typeCheck e e1 (hd (argTypes opType))) e2 ((hd o tl o argTypes) opType)
	typeCheck e=:{e=env} (Op1 op1 e1) type
	# (env, opType) = let type = e.o1 op1 in freeIds (env, type) (allIds type) // refresh type vars
	# e = typeCheck { e & e = env } (returnType opType) type
	= typeCheck e e1 (hd (argTypes opType))	
	typeCheck e (EInt _) type = typeCheck e TInt type
	typeCheck e (EBrace exp) type = typeCheck e exp type
	typeCheck e EBlock type = typeCheck e (TList (TId "t")) type
	typeCheck ue =: { e = e} (Tup a1 a2) (TTup (b1, b2)) = { ue3 & e = { ue3.e & subs = e.subs } } where 
		ue2 = (typeCheck ue a2 b2)
		ue3 = typeCheck { ue2 & e = { e & subs = e.subs } } a1 b1
	typeCheck e EFalse type = typeCheck e TBool type
	typeCheck e ETrue type = typeCheck e TBool type	
	typeCheck e=:{ types = types } (Alg c []) (TAlg n p) // Typechecks Algebraic Constructor Expressions without argument
	# (decl, types) = types getIndexed (n, { adname = "", poly = [], parts = [] })
	= case (algCT decl c) of
		(Just TEmpty) = { e & types = types }
		_ = { e & console = e.console <<< ("Constructor " +++ c +++ " does not exists or requires an argument"), error = True, types = types }
	// TODO declaredTypeFor id must check it type
	typeCheck e=:{ types = types } (Alg c [exp]) (TAlg n p) // Typechecks Algebraic Constructor Expressions with argument
	# (decl, types) = types getIndexed (n, { adname = "", poly = [], parts = [] })
	= case (algCT decl c) of // TODO apply consisten renaming of the type parameters
		(Just t) = typeCheck { e & types = types } exp ((algRewrite decl p) t)
		_ = { e & console = e.console <<< ("Constructor " +++ c +++ " does not exists for type " +++ n), error = True, types = types }
	typeCheck ue =: { e = e } (EFC f) type
	# (funType, uea) = (typeFor ue f.callName)
	# (env1, freshFunType) = freeIds (e, funType) (allIds funType) // TODO use locals?
	# aTypes = argTypes freshFunType
	| isTEmpty funType = (typingError uea (f.callName +++ " undefined"))
	| length f.callArgs <> length aTypes = (typingError uea (f.callName +++ " used with wrong arity"))
	# e1 = { uea & e = env1 }
	# e2 = typeCheck e1 (returnType freshFunType) type  // e2 may have new restrictions in typeVars
	# ue2 = foldl tupleCheck e2 [(a, b) \\ a <- f.callArgs & b <- aTypes]
	= { ue2 & e = { ue2.e & subs = e.subs } }	
	typeCheck e p t = typingError e ((toString p) +++ " does not match " +++ (toString t))

freeIds = foldl (g) where g (ue3, t) i = let (ue4, fresh) = (freshId ue3) in (ue4, replaceId i (TId fresh) t)
derive gEq Type, RetType
