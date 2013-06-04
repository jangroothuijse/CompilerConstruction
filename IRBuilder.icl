implementation module IRBuilder

import StdEnv, Parser, SemanticAnalyzer

Start = 0

// TODO overloading for toIR and toExp

:: IRInfo :== ([Decl], [FArg], [VarDecl])

toIR :: Prog -> IR
toIR prog = toIRDecl [] prog

toIRDecl :: [Decl] [Decl] -> [IRFun]
toIRDecl mainDecls [var=:(V _):xs]	= toIRDecl (mainDecls ++ [var]) xs // TODO: We probably have to put the main function at the end (and start with a jump instruction to it) to keep lazyness.
toIRDecl mainDecls [mainDecl=:(F {funName = "main"}):xs] = toMain (mainDecls ++ [mainDecl])
toIRDecl mainDecls [F { funName = name, args = args, vars = vars, stmts = stmts }:xs]  = [{ IRFun | name = name, blocks = (toBlockStmts (mainDecls, args, vars) name stmts)}:(toIRDecl mainDecls xs)]
toIRDecl mainDecls [] = abort "no main"

toIRBlock :: Block -> IRFun
toIRBlock block=:{Block | name = name} = {IRFun | name = name, blocks = [block]}

toIRLocVarDecls :: IRInfo -> [Command]
toIRLocVarDecls inf=:(mainDecls, args, vars=:[va:rs]) = [Link (length vars):flatten [toIRLocVarDecl inf var x \\ var <- vars & x<-[1..]]]
toIRLocVarDecls (mainDecls, args, []) = [Link 0]

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
toExpExp2 inf (EInt int) = [Put int]
toExpExp2 inf EFalse = [Put 0]
toExpExp2 inf ETrue = [Put 1]
toExpExp2 inf (EBrace exp) = toExpExp inf exp
toExpExp2 inf (EFC f) = toExpFCall inf f
toExpExp2 inf EBlock = [EFCall "__createEBlock"]
toExpExp2 inf (Tup ex1 ex2) = (toExpExp inf ex1) ++ (toExpExp inf ex2) ++ [EFCall "__createTup"] ++ [Drope 2]

toExpFCall :: IRInfo FunCall -> [CExp]
toExpFCall inf { callName = name, callArgs = exps } = (flatten (map (toExpExp inf) exps)) ++ [EFCall name] ++ [Drope (length exps)]

toBlockStmts :: IRInfo Id [Stmt] -> [Block]
toBlockStmts inf=:(mainDecls, args, vars) name s
#varblock = toIRLocVarDecls inf
#(commands, blocks, _) = toBlockStmts s 0
=[{name = name, commands = varblock ++ commands ++ [CReturn]}:blocks] // TODO: add return only when required
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
	getLocal [] _ _ _ = abort "getLocal used incorrect" // Shouldn't happen
getGlobal :: [Decl] Id -> Int
getGlobal a b = getGlobal a b 0
where
	getGlobal [V {VarDecl | name = name}:xs] id i
	| id == name = i
	=getGlobal xs id (i+1)
	getGlobal [] _ _ = abort "getGlobal used incorrect" // Shouldn't happen

// TODO: generate main
toMain :: [Decl] -> IR
toMain mainDecls = [{name = "main", blocks = [{name = "main", commands = main}:blocks]}]
	where
	(main, blocks) = toMain mainDecls
	toMain :: [Decl] -> ([Command], [Block])
	toMain [V {exp=exp}:xs]
	#exp = toIRExp (mainDecls, [], []) exp
	#(main, blocks) = toMain xs
	= ([exp:Swap:main], blocks)
	toMain [F {funName = "main", args = args, vars = vars, stmts = stmts}:_]
	#[{commands = main}:blocks] = toBlockStmts (mainDecls, args, vars) "main" stmts
	=(main, blocks)




