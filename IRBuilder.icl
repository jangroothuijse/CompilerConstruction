implementation module IRBuilder

import StdEnv, Parser, SemanticAnalyzer

Start = 0

// TODO overloading for toIR and toExp

:: IRInfo :== ([Decl], [FArg], [VarDecl])

toIR :: Prog -> IR
toIR prog = toIRDecl [] prog

toIRDecl :: [Decl] [Decl] -> [IRFun]
toIRDecl mainDecls [var=:(V _):xs]	= toIRDecl (mainDecls ++ [var]) xs
toIRDecl mainDecls [mainDecl=:(F {funName = "main"}):xs] = toMain (mainDecls ++ [mainDecl])
toIRDecl mainDecls [F { funName = name, args = args, vars = vars, stmts = stmts }:xs]  = [{ IRFun | name = name, blocks = (toBlockStmts (mainDecls, args, vars) name stmts)}:(toIRDecl mainDecls xs)]
toIRDecl mainDecls [alg=:(A _):xs] = toIRDecl (mainDecls ++ [alg]) xs
toIRDecl mainDecls [] = abort "no main"

toIRBlock :: Block -> IRFun
toIRBlock block=:{Block | name = name} = {IRFun | name = name, blocks = [block]}

toIRLocVarDecls :: IRInfo -> [Command]
toIRLocVarDecls inf=:(_, _, vars=:[va:rs]) = [Link (length vars):flatten [toIRLocVarDecl inf var x \\ var <- vars & x<-[1..]]]
toIRLocVarDecls _ = [Link 0]

toIRLocVarDecl :: IRInfo VarDecl Int -> [Command]
toIRLocVarDecl inf {exp = exp} i = [toIRExp inf exp, CAssingl i]

toIRExps :: IRInfo [Exp] -> [Command]
toIRExps inf [] = []
toIRExps inf [ex:ps] = [toIRExp inf ex] ++ toIRExps inf ps

toIRExp :: IRInfo Exp -> Command
toIRExp inf exp = CExp (toExpExp inf exp)

toExpExp :: IRInfo Exp -> [CExp]
toExpExp inf { ex = exp } = toExpExp2 inf exp

toExpExp2 :: IRInfo Exp2 -> [CExp]
toExpExp2 (mainDecls, args, vars) (I name)
|isGlobal mainDecls name = [Read (getGlobal mainDecls name)]
=[Readl (getLocal args vars name)]
toExpExp2 inf (Op2 ex1 op2 ex2) = (toExpExp inf ex1) ++ (toExpExp inf ex2) ++ [EOp2 op2]
toExpExp2 inf (Op1 op1 exp) = (toExpExp inf exp) ++ [EOp1 op1]
toExpExp2 _ (EInt int) = [Put int]
toExpExp2 _ EFalse = [Put 0]
toExpExp2 _ ETrue = [Put 1]
toExpExp2 inf (EBrace exp) = toExpExp inf exp
toExpExp2 inf (EFC f) = toExpFCall inf f
toExpExp2 _ EBlock = [EFCall "__createEBlock"]
toExpExp2 inf (Tup ex1 ex2) = (toExpExp inf ex1) ++ (toExpExp inf ex2) ++ [EFCall "__createTup"] ++ [Drope 2]
toExpExp2 inf=:(mainDecls, _, _) (Alg id [exp]) = [Put (getAlgNr mainDecls id)] ++ (toExpExp inf exp) ++ [EFCall "__createAlg"] ++ [Drope 2]
toExpExp2 (mainDecls, _, _) (Alg id []) = [Put (getAlgNr mainDecls id), Put 0] ++ [EFCall "__createAlg"] ++ [Drope 2]

toExpFCall :: IRInfo FunCall -> [CExp]
toExpFCall inf { callName = name, callArgs = exps } = (flatten (map (toExpExp inf) exps)) ++ [EFCall name] ++ [Drope (length exps)]

toBlockStmts :: IRInfo Id [Stmt] -> [Block]
toBlockStmts inf=:(mainDecls, args, vars) name s
#varblock = toIRLocVarDecls inf
#(commands, blocks, _) = toBlockStmts s 0
=[{name = name, commands = varblock ++ commands ++ [CReturn]}:blocks]
	where
	toBlockStmts :: [Stmt] Int -> ([Command], [Block], Int)
	toBlockStmts [x:xs] i
	#(commands, blocks, i) = toBlockStmt x i
	#(commands`, blocks`, i) = toBlockStmts xs i
	=(commands ++ commands`, blocks ++ blocks`, i)
	toBlockStmts [] i = ([], [], i)
	toBlockStmt :: Stmt Int -> ([Command], [Block], Int)
	toBlockStmt (Block stmt) i = toBlockStmts stmt i
	toBlockStmt (If exp stmt) i
	#(id, i) = getId name i
	#(id`, i) = getId name i
	#exp = toIRExp inf exp
	#(commands, blocks, i) = toBlockStmts [stmt] i
	=([exp, BranchIf id, Label id`],[{ name = id, commands = commands ++ [Branch id`]}:blocks], i)
	toBlockStmt (Ife exp stmt1 stmt2) i
	#(id, i) = getId name i
	#(id`, i) = getId name i
	#(id``, i) = getId name i
	#exp = toIRExp inf exp
	#(commands, blocks, i) = toBlockStmts [stmt1] i
	#(commands`, blocks`, i) = toBlockStmts [stmt2] i
	=([exp, BranchIfElse id id`, Label id``],[{ name = id, commands = commands ++ [Branch id``]}:{ name = id`, commands = commands` ++ [Branch id``]}:blocks], i)
	toBlockStmt (While exp stmt) i
	#(id, i) = getId name i
	#(id`, i) = getId name i
	#exp = toIRExp inf exp
	#(commands, blocks, i) = toBlockStmts [stmt] i
	=([ Label id, exp, BranchIf id`],[{ name = id`, commands = commands ++ [Branch id]}:blocks], i)
	toBlockStmt (Ass id exp) i
	#exp = toIRExp inf exp
	|isLocal args vars id
		=([exp, CAssingl (getLocal args vars id)],[], i)
	=([exp, CAssing (getGlobal mainDecls id)],[], i)
	toBlockStmt (SFC funCall) i
	#(id, i) = getId name i
	#exp = toIRExps inf funCall.callArgs
	=(exp ++ [CFCall funCall.callName] ++ [Drop (length funCall.callArgs)],[], i)
	toBlockStmt Return i
	=([CReturn], [], i)
	toBlockStmt (Returne exp) i
	#exp = toIRExp inf exp
	|(length vars)==0 = ([exp, CReturne], [], i)
	=([exp, CReturne], [], i)	
	toBlockStmt (Match var cases) i
	#exp = CExp (toExpExp2 inf (I var))
	#(cases, blocks, i) = toIRCases inf var cases i
	=([exp, CFCall "fst", BranchMatch cases], blocks, i)

toIRCases :: IRInfo Id [Case] Int -> ([(Int, Id)], [Block], Int)
toIRCases inf=:(mainDecls, _, _) tvar [Case name var stmt:xs] i
#(id, i) = getId name i
#(cases, blocks, i) = toIRCases inf tvar xs i
#block = toIRCaseBlock inf stmt var tvar id
=([(getAlgNr mainDecls name, id):cases], block ++ blocks, i)
toIRCases _ _ [] i = ([], [], i)

toIRCaseBlock :: IRInfo Stmt [Id] Id Id -> [Block]
toIRCaseBlock inf stmt [] tvar blockName = toBlockStmts inf blockName [stmt]
toIRCaseBlock inf stmt [var] tvar blockName = toBlockStmts inf blockName [c stmt]
	where
	// replace var in block with snd(tvar)
	c (Block stmt) = Block (map c stmt)
	c (If exp stmt) = If (ce exp) (c stmt)
	c (Ife exp stmt1 stmt2) = Ife (ce exp) (c stmt1) (c stmt2)
	c (While exp stmt) = While (ce exp) (c stmt)
	c (Ass id exp) = Ass id (ce exp)
	c (Returne exp) = Returne (ce exp)
	c (Match id cases) = Match id (map cc cases)
	c (SFC f) = SFC {f & callArgs = map ce f.callArgs}
	c x = x
	ce e = {e & ex = (ce2 e.ex)}
	ce2 (I id)
	|id == var = EFC { callName = "snd", callArgs = [{ex = I tvar, eline = 0, ecolumn = 0}]}
	= I id
	ce2 (EFC f) = EFC {f & callArgs = map ce f.callArgs}
	ce2 (Op2 exp1 op2 exp2) = Op2 (ce exp1) op2 (ce exp2)
	ce2 (Op1 op1 exp) = Op1 op1 (ce exp)
	ce2 (EBrace exp) = EBrace (ce exp)
	ce2 (Tup exp1 exp2) = Tup (ce exp1) (ce exp2)
	ce2 (Alg id exps) = Alg id (map ce exps)
	ce2 x = x
	cc (Case id ids stmt) = Case id ids (c stmt)

getAlgNr :: [Decl] Id -> Int
getAlgNr [V _:xs] type = getAlgNr xs type
getAlgNr [F _:xs] type = getAlgNr xs type
getAlgNr [A {adname = name, parts = parts}:xs] type = getAlgNr` 0 parts xs
	where
	getAlgNr` i [{apname = name`}:xs] xss
	|name` == name = i
	= getAlgNr` (i+1) xs xss
	getAlgNr` _ [] xss = getAlgNr xss type
getAlgNr _ x = abort ("getAlgNr used incorrect: " +++ x) // Shouldn't happen

getId :: String Int -> (String, Int)
getId s i = ("_" +++ (toString i) +++ "_" +++ s, i+1)

isLocal :: [FArg] [VarDecl] Id -> Bool
isLocal [{FArg|argName = idx}:rg] decl id
|idx==id	= True
=isLocal rg decl id
isLocal [] [{VarDecl|name = idx}:cl] id
|idx==id = True
=isLocal [] cl id
isLocal [] [] id = False

isGlobal :: [Decl] Id -> Bool
isGlobal [V {VarDecl | name = name}:xs] id
| id == name = True
=isGlobal xs id
isGlobal _ _ = False

getLocal :: [FArg] [VarDecl] Id -> Int
getLocal a b c = getLocal a b c (-1-(length a))
where
	getLocal [{FArg|argName = idx}:rg] decl id i
	|idx==id	= i
	=getLocal rg decl id (i + 1)
	getLocal [] [{VarDecl|name = idx}:cl] id i
	|idx==id = (i + 2)
	=getLocal [] cl id (i + 1)
	getLocal _ _ id _ = abort ("getLocal used incorrect: " +++ id) // Shouldn't happen
getGlobal :: [Decl] Id -> Int
getGlobal a b = getGlobal a b 0
where
	getGlobal [V {VarDecl | name = name}:xs] id i
	| id == name = i
	=getGlobal xs id (i+1)
	getGlobal [A _:xs] id i = getGlobal xs id i
	getGlobal [] _ _ = abort "getGlobal used incorrect" // Shouldn't happen

toMain :: [Decl] -> IR
toMain mainDecls = [{name = "main", blocks = [{name = "main", commands = main}:blocks]}]
	where
	(main, blocks) = toMain mainDecls
	toMain :: [Decl] -> ([Command], [Block])
	toMain [V {exp=exp}:xs]
	#exp = toIRExp (mainDecls, [], []) exp
	#(main, blocks) = toMain xs
	= ([exp:Swap:main], blocks)
	toMain [A _:xs] = toMain xs
	toMain [F {funName = "main", args = args, vars = vars, stmts = stmts}:_]
	#[{commands = main}:blocks] = toBlockStmts (mainDecls, args, vars) "main" stmts
	=(main, blocks)




