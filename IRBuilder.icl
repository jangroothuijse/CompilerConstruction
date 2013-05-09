implementation module IRBuilder

import StdEnv, Parser, SemanticAnalyzer

Start = 0

// TODO overloading for toIR and toExp

toIR :: (Prog, *UEnv) -> (IR, *UEnv)
toIR (prog, ue=:{e = env}) = ((flatten (map (toIRDecl env) prog)) ++ (toMain prog), ue)
	
toIRDecl :: Env Decl -> [IRFun]
toIRDecl env (F { funName = "main"}) = []
toIRDecl env (F { funName = name, args = args, vars = vars, stmts = stmts }) = [{ IRFun | name = name, blocks = (toIRVarDecls args vars) ++ (toBlockStmts env args vars name stmts)}]
toIRDecl env (V var) = [toIRBlock env (toIRVarDecl [] [] var)]

toIRBlock :: Env Block -> IRFun
toIRBlock env block=:{Block | name = name} = {IRFun | name = name, blocks = [block]}

toIRVarDecls :: [FArg] [VarDecl] -> [Block]
toIRVarDecls args vars = map (toIRVarDecl args vars) vars

toIRVarDecl :: [FArg] [VarDecl] VarDecl -> Block
toIRVarDecl args vars {name = name, exp = exp} = {Block | name = "$" +++ name, commands = toIRExp args (takeWhile (\x=x.VarDecl.name <> name) vars) exp, depth = 0}

toIRExp :: [FArg] [VarDecl] Exp -> [Command]
toIRExp args vars exp = [CExp (toExpExp args vars exp)]

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

toBlockStmts :: Env [FArg] [VarDecl] CId [Stmt] -> [Block]
toBlockStmts env args vars name s
#(commands, blocks, _) = toBlockStmts s 0 0
=[{name = name, commands = commands, depth = 0}:blocks]
	where
	toBlockStmts :: [Stmt] Int Int -> ([Command], [Block], Int)
	toBlockStmts [x:xs] depth i
	#(commands, blocks, i) = toBlockStmt x depth i
	#(commands`, blocks`, i) = toBlockStmts xs depth i
	=(commands ++ commands`, blocks ++ blocks`, i)
	toBlockStmt :: Stmt Int Int -> ([Command], [Block], Int)
	toBlockStmt (Block stmt) depth i
	#id = name +++ "$"  +++ (toString i)
	#(commands, blocks, i) = toBlockStmts stmt (depth + 1) (i+1)
	= ([Branch id], [{ name = id, commands = commands, depth = depth + 1}:blocks], i)
// If Exp Stmt | Ife Exp Stmt Stmt | While Exp Stmt | Ass Id Exp | SFC FunCall | Return | Returne Exp	

// generate main
toMain p = []