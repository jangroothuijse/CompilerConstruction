module CompilerTest

import StdEnv, Tokenizer, Parser, PrettyPrinter, StdList
//from Tokenizer import :: Token, :: Symbol, :: Operator

toLines :: String -> Result [String]
toLines s = {result = (toLines_ (fromString s)), errors = []}
where
	toLines_ [] = []
	toLines_ ['\n':s] = toLines_ s
	toLines_ s = [toString x:xs]
	where
		x = takeWhile (\x=x<>'\n') s
		xs = toLines_ (dropWhile (\x=x<>'\n') s)

Start
#(myProg, _) = progTest (newEnv 5)
#parseResult = parse (tokenizer (toLines (pretty 0 myProg)))
|not (isEmpty parseResult.errors) = (False, (pretty 0 myProg), parseResult.errors)
#progResult = fromJust parseResult.result
= (progResult === myProg, (pretty 0 myProg), parseResult.errors)

/*
Start = randoms (newEnv 7) 50
randoms e 0 = []
randoms e x
#(r, e) = randomD e
= [r:randoms e (x-1)]
*/
//Start = progTest (newEnv 6)
//Start = lesstest 3
//Start = test 2

test x = filter (\x=x.errors==[]) (map (\x=parse {result = x, errors = []}) (map (map \x={token = x, line = 0, column = 0}) (randomTokens x ++ testStmt x ++ [[KInt, Identifier "fun", POpen, PClose, CBOpen, KReturn, Semicolon, CBClose]])))
testStmt = append [KInt, Identifier "fun", POpen, PClose, CBOpen] [CBClose]

append a b i = map (\(x, y, z) = x ++ y ++ z) [(a, t, b) \\ t <- randomTokens i]

randomTokens :: Int -> [[Symbol]]
randomTokens 0 = [[POpen], [PClose], [CBOpen], [CBClose], [SBOpen], [SBClose], [Comma,
						Semicolon], [Identifier "x"], [Integer 0], [Op Plus], [Op Min,
						Op Mul], [Op Div], [Op Mod], [Op Eq], [Op LT], [Op GT], [Op LTE,
						Op GTE], [Op NEq], [Op And], [Op Or], [Op Cons], [Op Not], [KIf,
						KElse], [KWhile], [KReturn], [KVoid], [KInt], [KBool], [KTrue,
						KFalse], [KAssign]]
randomTokens i = map (\(x, y) = [x:y]) [(x,y) \\ x <- alltokens, y <- randomTokens (i-1)]
where
	alltokens = 		[POpen, PClose, CBOpen, CBClose, SBOpen, SBClose, Comma,
						Semicolon, Identifier "x", Integer 0, Op Plus, Op Min,
						Op Mul, Op Div, Op Mod, Op Eq, Op LT, Op GT, Op LTE,
						Op GTE, Op NEq, Op And, Op Or, Op Cons, Op Not, KIf,
						KElse, KWhile, KReturn, KVoid, KInt, KBool, KTrue,
						KFalse, KAssign]


lesstest x = filter (\x=x.errors==[]) (map (\x=parse {result = x, errors = []}) (map (map \x={token = x, line = 0, column = 0}) (lessRtokens x ++ lesstestStmt x ++ [[KInt, Identifier "fun", POpen, PClose, CBOpen, KReturn, Semicolon, CBClose]])))
lesstestStmt = lessappend [KInt, Identifier "fun", POpen, PClose, CBOpen] [CBClose]

lessappend a b i = map (\(x, y, z) = x ++ y ++ z) [(a, t, b) \\ t <- lessRtokens i]

lessRtokens ::  Int -> [[Symbol]]
lessRtokens 0 = [[], [CBClose]]
lessRtokens i = map (\(x, y) = [x:y]) [(x,y) \\ x <- alltokens, y <- randomTokens (i-1)]
where
	alltokens = 		[POpen, PClose, CBOpen, CBClose, 
						Semicolon, Identifier "x", Integer 0, KIf,
						KElse, KWhile, KReturn, KVoid, KInt, KBool, KTrue,
						KFalse, KAssign]



//randomTokensStmt = []

:: Env = {var :: Int, fun :: Int, seed :: Int}

newEnv i = e
where
	(_, e) = random {var = 0, fun = 0, seed = i}

progTest :: Env -> (Prog, Env)
progTest e
#(rand, e) = randomD e
|rand > 0.5
	#(l1, e) = progTestVar e
	#(l2, e) = progTest e
	= ([l1: l2], e)
|rand > 0.1
	#(l1, e) = progTestFun e
	#(l2, e) = progTest e
	= ([l1: l2], e)
= ([], e)
 
progTestVar e = (V { type = TInt, name = "v" +++ toString e.var, exp = EInt 0}, {e & var = e.var + 1})

progTestFun e = (F {retType = TVoid, funName = "f" +++ toString e.fun, args = [], vars = [], stmts = [Return]}, {e & fun = e.fun + 1})

randomD :: Env -> (Real, Env)
randomD e = ((toReal x) / 65536.0, y)
where
	(x, y) = random e

random :: Env -> (Int, Env) 
random e = (newSeed, {e & seed = newSeed})
where
	newSeed		= if (nextSeed>=0) nextSeed (nextSeed+65537)
	nextSeed	= (seed75 bitand 65535)-(seed75>>16)
	seed75		= e.seed*75
