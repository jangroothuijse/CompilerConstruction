implementation Parser

:: Result a = { result :: a, errors :: [String], input :: [TokenOnLine] }
:: ParseResult :== Result AST

:: AST :== [Decl]
:: Decl = VarDecl VarDecl | FunDecl FunDecl
:: VarDecl = { varType :: VarType, name :: String, expr :: Expr }
:: FunDecl = { returnType :: ReturnType, name :: String, args :: [ArgDecl], vars :: [VarDecl], body :: [Stmt] }
:: Stmt = Stmt
:: VarType = VTInt | VTBool | VTTuple VarType VarType | VTList VarType | VTA String
:: ReturnType = RT VarType | RTVoid

funDecl :: ReturnType String [ArgDecl] [VarDecl] [Stmt] -> FunDecl
funDecl returnType name args vars body = { returnType = returnType, name = id, args = args, vars = vars, body = body }

varDecl :: VarType String Expr -> VarDecl
varDecl varType name expr = 

:: Parser  :== [TokenOnLine] -> ParseResult
parse :: Parser
parse input = f input [] []
where
	f [] decl errors = { result = decl, errors = errors, input = [] }
	f input decl errors = f r.input [r.result:decl] [r.errors:errors]
	where r = parseDecl input

parseDecl :: ([TokenOnLine] -> Result Decl)
parseDecl = parseName o parseType
where 
		parseDecl2 :: (Result ReturnType) -> Result Decl 
		parseDecl2 result=:{ input = [Identifier name: xs]} = parseDeclKind xs result.result name 
		parseDecl2 x			= abort "No, sorry"
		
		parseDecl3 :: [TokenOnLine] ReturnType String -> Result Decl
		parseDecl3 [Kassign:xs] (RT varType) name = { result = VarDecl { varType = varType, name = name, expr = expr }, errors = [], input = xs }
		where 
			
		parseDecl3 [Kassign:xs] (RTVoid) name = abort "Variables cannot have type Void"
		parseDecl3 [POpen:xs] returnType name 
		
where
	parseDecl2 

//parseVarDecl :: [TokenOnLine]


/**
 * 
	Prog => Decl
	Decl => Type
*/
