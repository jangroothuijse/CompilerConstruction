implementation module IRBuilder

import StdEnv, Parser

Start = 0

// TODO overloading for toIR and toExp

toIR :: Prog -> IR
toIR prog = (flatten (map toIRDecl prog)) ++ (toMain prog)
	
toIRDecl :: Decl -> [IRFun]
toIRDecl (F { funName = "main"}) = []
toIRDecl (F { funName = name, args = args, vars = vars, stmts = stmts }) = [{ IRFun | name = name, blocks = (toBlockArgs args) ++ (toIRVarDecls vars) ++ (toBlockStmts name 0 stmts)}]
toIRDecl (V var) = [toIRBlock (toIRVarDecl var)]

toIRBlock :: Block -> IRFun
toIRBlock block=:{Block | name = name} = {IRFun | name = name, blocks = [block]}

toIRVarDecls :: [VarDecl] -> [Block]
toIRVarDecls vars = map toIRVarDecl vars

toIRVarDecl :: VarDecl -> Block
toIRVarDecl {name = name, exp = exp} = {Block | name = "$" +++ name, commands = toIRExp exp}

toIRExp :: Exp -> [Command]
toIRExp { ex = exp } = [CExp (toExpExp2 exp)]

toExpExp :: Exp -> [CExp]
toExpExp { ex = exp } = toExpExp2 exp

toExpExp2 :: Exp2 -> [CExp]
toExpExp2 (I name) = [Read name]
toExpExp2 (Op2 ex1 op2 ex2) = (toExpExp ex1) ++ (toExpExp ex2) ++ (toExpOp2 op2)
toExpExp2 (Op1 op1 exp) = (toExpExp exp) ++ (toExpOp1 op1)
toExpExp2 (EInt int) = [Put int]
toExpExp2 EFalse = [Put 0]
toExpExp2 ETrue = [Put 1]
toExpExp2 (EBrace exp) = toExpExp exp
toExpExp2 (EFC f) = toExpFCall f
//toExpExp2 EBlock = createEblock
//toExpExp2 Tup ex1 ex2 = (toExpExp ex1) ++ (toExpExp ex2) ++ createTup

toExpFCall :: FunCall -> [CExp]
toExpFCall { callName = name, callArgs = exp } = (flatten (map toExpExp exp)) ++ [EFCall name]

toBlockArgs :: [FArg] -> [Block]
toBlockArgs [{ argName = name }:xs] = [{ name = name, commands = []}: toBlockArgs xs]
toBlockArgs [] = []

toBlockStmts :: CId Int [Stmt] -> [Block]
toBlockStmts name i s = blocks
	where
	(blocks, _) = toBlockStmts name i s
	toBlockStmts :: CId Int [Stmt] -> ([Block], Int)
	toBlockStmts name i [s:xs]
	#(block , i) = toBlockStmt  name i s // also return name of next stmt block
	#(blocks, i) = toBlockStmts name i xs
	=(block ++ blocks, i)
	toBlockStmts name i [] = ([], i)
	toBlockStmt :: CId Int Stmt -> ([Block], Int)
	toBlockStmt name i (Block stmts) = toBlockStmts name i stmts
//	toBlockStmt name i (If Exp Stmt) = 
// If Exp Stmt | Ife Exp Stmt Stmt | While Exp Stmt | Ass Id Exp | SFC FunCall | Return | Returne Exp

// generate main
toExpOp1 x = []
toExpOp2 x = []
toMain p = []