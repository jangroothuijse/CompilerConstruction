implementation module IRBuilder

import StdEnv, Parser, SemanticAnalyzer

Start = 0

// TODO overloading for toIR and toExp

toIR :: (Prog, *UEnv) -> (IR, *UEnv)
toIR (prog, ue=:{e = env}) = ((toMain prog) ++ (flatten (map (toIRDecl env) prog)), ue)

toIRDecl :: Env Decl -> [IRFun]
toIRDecl env (F { funName = "main"}) = []
toIRDecl env (F { funName = name, args = args, vars = vars, stmts = stmts }) = [{ IRFun | name = name, blocks = (toBlockStmts env args vars name stmts)}]
toIRDecl env (V var) = []
//toIRDecl env (V var) = [toIRBlock env (toIRVarDecl [] [] var)]

toIRBlock :: Env Block -> IRFun
toIRBlock env block=:{Block | name = name} = {IRFun | name = name, blocks = [block]}
/*
toIRVarDecls :: [FArg] [VarDecl] -> [Block]
toIRVarDecls args vars = map (toIRVarDecl args vars) vars

toIRVarDecl :: [FArg] [VarDecl] VarDecl -> Block
toIRVarDecl args vars {name = name, exp = exp} = {Block | name = "$" +++ name, commands = [toIRExp args (takeWhile (\x=x.VarDecl.name <> name) vars) exp], depth = 0}
*/
toIRLocVarDecls :: [FArg] [VarDecl] -> [Command]
toIRLocVarDecls args vars=:[va:rs] = [Link (length vars):flatten [toIRLocVarDecl args vars var x \\ var <- vars & x<-[0..]]]
toIRLocVarDecls args [] = []

toIRLocVarDecl :: [FArg] [VarDecl] VarDecl Int -> [Command]
toIRLocVarDecl args vars {exp = exp} i = [toIRExp args vars exp, CAssingl i]

toIRExps :: [FArg] [VarDecl] [Exp] -> [Command]
toIRExps args vars [] = []
toIRExps args vars [ex:ps] = [toIRExp args vars ex] ++ toIRExps args vars ps

toIRExp :: [FArg] [VarDecl] Exp -> Command
toIRExp args vars exp = CExp (toExpExp args vars exp)

toExpExp :: [FArg] [VarDecl] Exp -> [CExp]
toExpExp args vars { ex = exp } = toExpExp2 args vars exp

toExpExp2 :: [FArg] [VarDecl] Exp2 -> [CExp]
toExpExp2 args vars (I name) = [Read name] // TODO Read or Readl?
toExpExp2 args vars (Op2 ex1 op2 ex2) = (toExpExp args vars ex1) ++ (toExpExp args vars ex2) ++ [EOp2 op2]
toExpExp2 args vars (Op1 op1 exp) = (toExpExp args vars exp) ++ [EOp1 op1]
toExpExp2 _ _ (EInt int) = [Put int]
toExpExp2 _ _ EFalse = [Put 0]
toExpExp2 _ _ ETrue = [Put 1]
toExpExp2 args vars (EBrace exp) = toExpExp args vars exp
toExpExp2 args vars (EFC f) = toExpFCall args vars f
toExpExp2 _ _ EBlock = [EFCall "$$createEBlock"]
toExpExp2 args vars (Tup ex1 ex2) = (toExpExp args vars ex1) ++ (toExpExp args vars ex2) ++ [EFCall "$$createTup"]

toExpFCall :: [FArg] [VarDecl] FunCall -> [CExp]
toExpFCall args vars { callName = name, callArgs = exp } = (flatten (map (toExpExp args vars) exp)) ++ [EFCall name]

toBlockStmts :: Env [FArg] [VarDecl] Id [Stmt] -> [Block]
toBlockStmts env args vars name s
#varblock = toIRLocVarDecls args vars
#(commands, blocks, _) = toBlockStmts s (length vars) 0
=[{name = name, commands = varblock ++ commands, depth = 0}:blocks]
	where
	toBlockStmts :: [Stmt] Int Int -> ([Command], [Block], Int)
	toBlockStmts [x:xs] depth i
	#(commands, blocks, i) = toBlockStmt x depth i
	#(commands`, blocks`, i) = toBlockStmts xs depth i
	=(commands ++ commands`, blocks ++ blocks`, i) // TODO: add return after void function
	toBlockStmt :: Stmt Int Int -> ([Command], [Block], Int)
	toBlockStmt (Block stmt) depth i
	#id = name +++ "$"  +++ (toString i)
	#(commands, blocks, i) = toBlockStmts stmt (depth + 1) (i+1)
	=([Branch id], [{ name = id, commands = commands, depth = depth + 1}:blocks], i)
	toBlockStmt (If exp stmt) depth i
	#id = name +++ "$"  +++ (toString i)
	#exp = toIRExp args vars exp
	#(commands, blocks, i) = toBlockStmts [stmt] (depth + 1) (i+1)
	=([exp, BranchIf id],[{ name = id, commands = commands, depth = (depth + 1)}:blocks], i)
	toBlockStmt (Ife exp stmt1 stmt2) depth i
	#id = name +++ "$"  +++ (toString i)
	#exp = toIRExp args vars exp
	#(commands, blocks, i) = toBlockStmts [stmt1] (depth + 1) (i+1)
	#id` = name +++ "$"  +++ (toString i)
	#(commands`, blocks`, i) = toBlockStmts [stmt2] (depth + 1) (i+1)
	=([exp, BranchIfElse id id`],[{ name = id, commands = commands, depth = (depth + 1)}:{ name = id`, commands = commands`, depth = (depth + 1)}:blocks], i)
	toBlockStmt (While exp stmt) depth i
	#id = name +++ "$"  +++ (toString i)
	#exp = toExpExp args vars exp
	#(commands, blocks, i) = toBlockStmts [stmt] (depth + 1) (i+1)
	=([BranchWhile exp id],[{ name = id, commands = commands, depth = (depth + 1)}:blocks], i)
	toBlockStmt (Ass id exp) depth i
	#exp = toIRExp args vars exp
	|isLocal args vars id
		=([exp, CAssingl (getLocal args vars id (~depth-(length args)))],[], i)
	=([exp, CAssing id],[], i)
	toBlockStmt (SFC funCall) depth i
	#id = name +++ "$"  +++ (toString i)
	#exp = toIRExps args vars funCall.callArgs
	=(exp ++ [CFCall funCall.callName],[], i)
	toBlockStmt Return depth i
	|(length vars)==0 = ([CReturn], [], i)
	=([Unlink, CReturn], [], i)
	toBlockStmt (Returne exp) depth i
	#exp = toIRExp args vars exp
	|(length vars)==0 = ([exp, CReturn], [], i)
	=([Unlink, exp, CReturne], [], i)	

isLocal :: [FArg] [VarDecl] Id -> Bool
isLocal [{FArg|argName = idx}:rg] decl id
|idx==id	= True
=isLocal rg decl id
isLocal [] [{VarDecl|name = idx}:cl] id
|idx==id = True
=isLocal [] cl id
isLocal [] [] id = False

getLocal :: [FArg] [VarDecl] Id Int -> Int
getLocal [{FArg|argName = idx}:rg] decl id i
|idx==id	= i
=getLocal rg decl id (i + 1)
getLocal [] [{VarDecl|name = idx}:cl] id i
|idx==id = i
=getLocal [] cl id (i + 1)
getLocal [] [] id i = abort ("local not found: " +++ id) // Shouldn't happen

// generate main
toMain p = []