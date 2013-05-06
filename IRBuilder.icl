implementation module IRBuilder

import StdEnv, Parser

Start = 0

toIR :: Prog -> IR
toIR prog = (flatten (map toIRDecl prog)) ++ (toMain prog)
	
toIRDecl :: Decl -> [IRFun]
toIRDecl (F { funName = "main"}) = []
toIRDecl (F { funName = name, args = args, vars = vars, stmts = stmts }) = [{ IRFun | name = name, blocks = (toBlockArgs args) ++ (toIRVarDecls vars) ++ (toBlockStmts stmts)}]
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
toExpExp2 (Op1 op1 exp) = (toExpExp exp) ++ (toExpOp2 op1)
toExpExp2 (EInt int) = [Put int]
toExpExp2 EFalse = [Put 0]
toExpExp2 ETrue = [Put 1]
toExpExp2 (EBrace exp) = toExpExp exp
toExpExp2 (EFC f) = toExpFCall f
//toExpExp2 EBlock = createEblock
//toExpExp2 Tup ex1 ex2 = (toExpExp ex1) ++ (toExpExp ex2) ++ createTup

// generate main
toExpOp2 x = []
toExpFCall f = []
toMain p = []
toBlockStmts s = []
toBlockArgs a = []
