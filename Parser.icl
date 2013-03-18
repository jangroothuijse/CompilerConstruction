implementation module Parser

import Result
import Tokenizer

parse :: (Result [TokenOnLine]) -> Result (Maybe Prog)
parse {result = r}
#(p, err, y) = parseProg r
|(isNothing p) = {result = Nothing, errors = err}
={result = p, errors = err}

parseProg :: [TokenOnLine] -> (Maybe Prog, [String], [TokenOnLine])
parseProg r=:[_:_]
#(t, e, rs) = parseDecls r
|(isNothing t) = (Nothing, e, rs)
= (Just (P (fromJust t)), e, rs)
parseProg [] = endOfFileError

parseDecls :: [TokenOnLine] -> (Maybe [Decl], [String], [TokenOnLine])
parseDecls r // Decl+
#(t, e, rs) = parseDecl r
|(isNothing t) = (Nothing, e, rs)
|(isEmpty rs) = (Just [fromJust t], e, rs) // Last Decl reached
#(t1, e1, rs) = parseDecls rs
|(isNothing t1) = (Just [fromJust t], e1 ++ e, rs) // return successfull tree // TBD: Skip failed part and parse next function
=(Just [fromJust t: fromJust t1], e1 ++ e, rs)

parseDecl :: [TokenOnLine] -> (Maybe Decl, [String], [TokenOnLine])
parseDecl r=:[_:_]
#(t, e, rs) = parseRetDecl r ~># parseId
|(isNothing t) = (Nothing, e, rs) ~>. cantParse r "VarDecl or FunDecl"
#(t1, t2) = fromJust t
#(t, e1, rs1) = parsePOpen rs
|(isNothing t) // VarDecl
	|(isPVoid t1) = cantParse r "FunDecl" rs1
	#(t3, e2, rs) = parseKAssign rs ~>- parseExp ~> parseSemicolon
	#(RT t1) = t1
	= (Just (V (VD t1 t2 (fromJust t3))), e2 ++ e1 ++ e, rs)
#(t, e2, rs) = parseFArgs_ rs1 ~> parsePClose ~> parseCBOpen ~># parseVarDecls_ ~># parseStmts ~> parseCBClose // FunDecl
|(isNothing t) = (Nothing, e2 ++ e1 ++ e, rs) ~>. cantParse r "FunDecl"
#((t3, t4), t5) = fromJust t
=(Just (F (Fun t1 t2 t3 t4 t5)), e, rs)
	where 
	isPVoid :: RetType -> Bool
	isPVoid PVoid = True
	isPVoid _ = False
parseDecl [] = (Nothing, [], [])


parseRetDecl :: [TokenOnLine] -> (Maybe RetType, [String], [TokenOnLine])
parseRetDecl [{token = KVoid}: rs] = (Just PVoid, [], rs)
parseRetDecl r
#(t, e, rs) = parseType r
|(isNothing t) = (Nothing, e, rs)
=(Just (RT (fromJust t)), e, rs)

parseType :: [TokenOnLine] -> (Maybe Type, [String], [TokenOnLine])
parseType [{token = KInt}: rs] = (Just TInt, [], rs)
parseType [{token = KBool}: rs] = (Just PBool, [], rs)
parseType [{token = POpen}: rs] // A tupel
#(t, e, rs) = parseType rs ~> parseComma ~># parseType ~> parsePClose
|(isNothing t) = (Nothing, e, rs)
#(t1, t2) = fromJust t
=(Just (TTup (t1, t2)), e, rs)
parseType r=:[{token = Identifier _}: rs]
#(t, e, rs) = parseId r
|(isNothing t) = (Nothing, e, rs)
=(Just (TId (fromJust t)), [], rs)
parseType [{token = SBOpen}: rs] // A list
#(t, e, rs) = parseType rs ~> parseSBClose
|(isNothing t) = (Nothing, e, rs)
=(Just (TList (fromJust t)), e, rs)
parseType [r:rs] = cantParse r "Type" rs
parseType [] = endOfFileError

parseFArgs_ :: [TokenOnLine] -> (Maybe [FArgs], [String], [TokenOnLine])
parseFArgs_ r=:[{token = PClose}: _] = (Just [], [], r) // FArgs*
parseFArgs_ r = parseFArgs r

parseFArgs :: [TokenOnLine] -> (Maybe [FArgs], [String], [TokenOnLine])
parseFArgs r // FArgs+
#(t, e, rs) = parseFArg r
|(isNothing t) = (Nothing, e, rs)
#(t1, e1, rs1) = parseComma rs
|(isNothing t1) = (Just [fromJust t], e, rs) // Last FArgs reached
#(t1, e2, rs) = parseFArgs rs1
|(isNothing t1)= (Nothing, e2 ++ e1 ++ e, rs)
=(Just [fromJust t:fromJust t1], e2 ++ e1 ++ e, rs)

parseFArg :: [TokenOnLine] -> (Maybe FArgs, [String], [TokenOnLine])
parseFArg r
#(t, e, rs) = parseType r ~># parseId
|(isNothing t) = (Nothing, e, rs)
#(t1, t2) = fromJust t
=(Just (FA t1 t2), e, rs)

parseVarDecls_ :: [TokenOnLine] -> (Maybe [VarDecl], [String], [TokenOnLine])
parseVarDecls_ r // VarDecl*
#(t, e, rs) = parseVarDecl r
|(isNothing t) = (Just [], [], r)// At this point backtracing is easier
#(t1, e1, rs) = parseVarDecls_ rs
=(Just [fromJust t:fromJust t1], e1 ++ e, rs)

parseVarDecl :: [TokenOnLine] -> (Maybe VarDecl, [String], [TokenOnLine])
parseVarDecl r
#(t, e, rs) = parseType r ~># parseId ~> parseKAssign ~># parseExp ~> parseSemicolon
|(isNothing t) =  (Nothing, e, rs) ~>. cantParse r "VarDecl"
#((t1, t2), t3) = fromJust t
= (Just (VD t1 t2 t3), e, rs)

parseStmts :: [TokenOnLine] -> (Maybe [Stmt], [String], [TokenOnLine])
parseStmts r // Stmt+
#(t, e, rs) = parseStmt r
|(isNothing t) = (Nothing, e, rs)
#(t1, e1, rs1) = parseStmts rs
|(isNothing t1) = (Just [fromJust t], e, rs)
=(Just [fromJust t: fromJust t1], e1 ++ e, rs1)

parseStmt :: [TokenOnLine] -> (Maybe Stmt, [String], [TokenOnLine])
parseStmt [{token = CBOpen}:rs] // Block
#(t, e, rs1) = parseCBClose rs
|(isNothing t) // Block with statements
	#(t, e, rs) = parseStmts rs ~> parseCBClose
	|(isNothing t) = (Nothing, e, rs)
	=(Just (Block (fromJust t)), e, rs)
=(Just (Block []), e, rs1) // Empty block
parseStmt [{token = KIf}:rs]
#(t, e, rs) = parsePOpen rs ~>- parseExp ~> parsePClose ~># parseStmt
|(isNothing t) = (Nothing, e, rs)
#(t1, t2) = fromJust t
|(isKElse rs)
	#(t, e1, rs) = parseKElse rs ~>- parseStmt
	|(isNothing t) = (Nothing, e ++ e1, rs)
	= (Just (Ife t1 t2 (fromJust t)), e1 ++ e, rs)
=(Just (If t1 t2), e, rs)
parseStmt [{token = KWhile}:rs]
#(t, e, rs) = parsePOpen rs ~>- parseExp ~> parsePClose ~># parseStmt
|(isNothing t) = (Nothing, e, rs)
#(t1, t2) = fromJust t
=(Just (While t1 t2), e, rs)
parseStmt r=:[{token = KReturn}: rs]
|(isSemicolon rs)
	#(t, e, rs) = parseSemicolon rs
	|(isNothing t) = (Nothing, e, rs)
	= (Just Return, e, rs)
#(t, e, rs) = parseExp rs ~> parseSemicolon
|(isNothing t) = (Nothing, e, rs)
= (Just (Returne (fromJust t)), e, rs)
parseStmt r=:[{token = Identifier _}: rs] // Funcall or Assign
#(t, e, rs) = parseId r
|(isPOpen rs)
	#(t1, e1, rs) = parsePOpen rs ~>- parseActArgs_ ~> parsePClose ~> parseSemicolon // Funcall
	|(isNothing t1) = (Nothing, e, rs)
	=(Just (SFC (FC (fromJust t) (fromJust t1))), e, rs)
#(t1, e1, rs) = parseKAssign rs ~>- parseExp ~> parseSemicolon // Assing
|(isNothing t) = (Nothing, e1 ++ e, rs)
=(Just (Ass (fromJust t) (fromJust t1)), e1 ++ e, rs)
parseStmt [r:rs] = cantParse r "Stmt" rs
parseStmt [] = endOfFileError

parseActArgs_ :: [TokenOnLine] -> (Maybe [ActArgs], [String], [TokenOnLine])
parseActArgs_ r=:[{token = PClose}: _] = (Just [], [], r) // ActArgs*
parseActArgs_ r = parseActArgs r

parseActArgs :: [TokenOnLine] -> (Maybe [ActArgs], [String], [TokenOnLine])
parseActArgs r // ActArgs+
#(t, e, rs) = parseExp r
|(isNothing t) = (Nothing, e, rs)
#(t1, e1, rs1) = parseComma rs
|(isNothing t1) = (Just [AA (fromJust t)], e1 ++ e, rs)
#(t1, e2, rs) = parseActArgs rs1
|(isNothing t1) = (Nothing, e2 ++ e1 ++ e, rs)
=(Just ([AA (fromJust t): fromJust t1]), e2 ++ e1 ++ e, rs)

/* 
 * ConsExp = RelExp ConsExp`
 * ConsExp = : ConsExp | lamda
 * RelExp = Exp RelExp` | (RelExp)
 * RelExp` = == RelExp | < RelExp | > RelExp | <= RelExp | >= RelExp | != RelExp | lamda
 * Exp = Term Exp`
 * Exp` = + Exp | - Exp | lamda
 * Term = Factor Term`
 * Term` = * Term | / Term | % Term | lamda
 * Factor = ! Factor | - Factor | Num | True | False | Id | FunCall
 */
 
parseExp :: ([TokenOnLine] -> (Maybe Exp, [String], [TokenOnLine]))
parseExp = parseConsExp

parseConsExp :: [TokenOnLine] -> (Maybe Exp, [String], [TokenOnLine])
parseConsExp rs
#	(t, e, rs)		= parseAndExp rs
|	not (isJust t) 	= (Nothing, ["Expression expected but not found on line " +++ (toString (hd rs).line):e], rs)
=	case rs of
	[{token = Op Cons}:rs] = if (isJust t2) (Just (Op2 (fromJust t) PCons (fromJust t2)), e ++ e2, rrs) (Nothing, e ++ e2, rrs)
	where (t2, e2, rrs) = parseConsExp rs
	_					= (t, e, rs)

parseAndExp :: [TokenOnLine] -> (Maybe Exp, [String], [TokenOnLine])
parseAndExp rs
#	(t, e, rs) 		= parseOrExp rs
|	isNothing t		= (Nothing, e, rs)
=	case rs of
	[{token = Op And}:rrs]	= if (isJust t2) (Just (Op2 (fromJust t) PAnd (fromJust t2)), e ++ e2, rs2) (Nothing, e, rs)	
	where (t2, e2, rs2) = parseAndExp rrs
	_				= (t, e, rs)
	
parseOrExp :: [TokenOnLine] -> (Maybe Exp, [String], [TokenOnLine])
parseOrExp rs
#	(t, e, rs) 		= parseRelExp rs
|	isNothing t		= (Nothing, e, rs)
=	case rs of
	[{token = Op Or}:rrs]	= if (isJust t2) (Just (Op2 (fromJust t) POr (fromJust t2)), e ++ e2, rs2) (Nothing, e, rs)	
	where (t2, e2, rs2) = parseOrExp rrs
	_				= (t, e, rs)

parseRelExp :: [TokenOnLine] -> (Maybe Exp, [String], [TokenOnLine])
parseRelExp rs
#	(t, e, rs) 		= parseSum rs
|	not (isJust t) 	= (Nothing, ["Expression expected but not found on line " +++ (toString (hd rs).line):e], rs)
#	exp1 			= fromJust t
= 	case rs of
	[{token = Op op}:rrs] = case op of
		Eq	= emitOperator PEq
		LT	= emitOperator PLT
		GT	= emitOperator PGT
		LTE	= emitOperator PLTE
		GTE = emitOperator PGTE
		NEq = emitOperator PNEq
		_ 	= (t, e, rs)
	where
		(t2, e2, rs2) = parseRelExp rrs
		emitOperator :: Op2 -> (Maybe Exp, [String], [TokenOnLine])
		emitOperator operator = if (isJust t2) (Just (Op2 exp1 operator (fromJust t2)), e ++ e2, rs2) (Nothing, e, rs)	
	_	=	(t, e, rs)

parseSum :: [TokenOnLine] -> (Maybe Exp, [String], [TokenOnLine])
parseSum rs
#	(t, e, rs)		= parseTerm rs
|	not (isJust t)	= (Nothing, ["Expression expected but not found on line " +++ (toString (hd rs).line):e], rs)
#	term1			= fromJust t
=	case rs of
	[{token = Op op}:rrs] = case op of
		Plus	= emitOperator PPlus
		Min		= emitOperator PMin
		_ 		= (t, e, rs)
	where
		(t2, e2, rs2) = parseSum rrs
		emitOperator :: Op2 -> (Maybe Exp, [String], [TokenOnLine])
		emitOperator operator = if (isJust t2) (Just (Op2 term1 operator (fromJust t2)), e ++ e2, rs2) (Nothing, e, rs)	
	_	=	(t, e, rs)	

parseTerm :: [TokenOnLine] -> (Maybe Exp, [String], [TokenOnLine])
parseTerm rs
#	(t, e, rs)		= parseFactor rs
|	not (isJust t)	= (Nothing, ["Expression expected but not found on line " +++ (toString (hd rs).line):e], rs)
#	fact1			= fromJust t
=	case rs of
	[{token = Op op}:rrs] = case op of
		Mul		= emitOperator PMul
		Div		= emitOperator PDiv
		Mod		= emitOperator PMod
		_ 		= (t, e, rs)
	where
		(t2, e2, rs2) = parseTerm rrs
		emitOperator :: Op2 -> (Maybe Exp, [String], [TokenOnLine])
		emitOperator operator = if (isJust t2) (Just (Op2 fact1 operator (fromJust t2)), e ++ e2, rs2) (Nothing, e, rs)	
	_	=	(t, e, rs)	
	
parseFactor :: [TokenOnLine] -> (Maybe Exp, [String], [TokenOnLine])
parseFactor [{token = Op Not}:rs]
#	(t, e, rs)		= parseFactor rs
|	isJust t		= (Just (Op1 PNot (fromJust t)), e, rs)
					= (Nothing, ["Expression expect on line " +++ (toString (hd rs).line):e], rs)
parseFactor [{token = Op Min}:rs]
#	(t, e, rs)		= parseFactor rs
|	isJust t		= (Just (Op1 PNeg (fromJust t)), e, rs)
					= (Nothing, ["Expression expect on line " +++ (toString (hd rs).line):e], rs)
parseFactor [{token = Integer z}:rs] = (Just (EInt z), [], rs)
parseFactor [{token = KTrue}:rs] = (Just ETrue, [], rs)
parseFactor [{token = KFalse}:rs] = (Just ETrue, [], rs)
parseFactor [{token = POpen}:rs] // Parentheses AND Tuples...
# 	(t, e, rs)		= parseExp rs
|	isJust t		= case rs of 
	[{token = Comma}:rrs] = case r.token of
		PClose	= if (isJust t2) (Just (Tup (fromJust t) (fromJust t2)), e ++ e2, rrrs) (Nothing, e ++ e2, rrrs)
		x = (Nothing, e ++ e2 ++ ["Expected tuple closing parenthesis"], rrrs)
	where (t2, e2, [r:rrrs]) = parseExp rrs
	[{token = PClose}:rrs] 	= (Just (EBrace (fromJust t)), e, rrs)
	_						= (Nothing, ["Failed to parse expression on line " +++ (toString (hd rs).line)], rs)
=	(t, e, rs)
parseFactor [{token = SBOpen}:rs] = case rs of 
	[{token = SBClose}: rrs]	= (Just EBlock, [], rrs)
	x							= (Nothing, ["Cannot parse expression"], x)
parseFactor x = parseIdAndCall x

parseIdAndCall :: [TokenOnLine] -> (Maybe Exp, [String], [TokenOnLine])
parseIdAndCall [{token = Identifier name}:rs] = case rs of
	[{token = POpen}:rrs]
	#(t, e,rs) = parsePOpen rs ~>- parseActArgs_ ~> parsePClose
	|(isNothing t) = (Nothing, e, rs)
	= (Just (EFC (FC (PId name) (fromJust t))), e, rs)
	_						= (Just (I (PId name)), [], rs)		// ID
parseIdAndCall x = (Nothing, ["Failed to parse expression on line " +++ (toString (hd x).line)], x)
 
parseSemicolon :: [TokenOnLine] -> (Maybe Bool, [String], [TokenOnLine])
parseSemicolon [{token = Semicolon}: rs] = (Just True, [], rs)
parseSemicolon [r: rs] = cantParse r "';'" rs
parseSemicolon [] = endOfFileError

parseKAssign :: [TokenOnLine] -> (Maybe Bool, [String], [TokenOnLine])
parseKAssign [{token = KAssign}: rs] = (Just True, [], rs)
parseKAssign [r: rs] = cantParse r "'='" rs
parseKAssign [] = endOfFileError

parseComma :: [TokenOnLine] -> (Maybe Bool, [String], [TokenOnLine])
parseComma [{token = Comma}: rs] = (Just True, [], rs)
parseComma [r: rs] = cantParse r "','" rs
parseComma [] = endOfFileError

parsePOpen :: [TokenOnLine] -> (Maybe Bool, [String], [TokenOnLine])
parsePOpen [{token = POpen}: rs] = (Just True, [], rs)
parsePOpen [r: rs] = cantParse r "'('" rs
parsePOpen [] = endOfFileError

parsePClose :: [TokenOnLine] -> (Maybe Bool, [String], [TokenOnLine])
parsePClose [{token = PClose}: rs] = (Just True, [], rs)
parsePClose [r: rs] = cantParse r "')'" rs
parsePClose [] = endOfFileError

parseCBOpen :: [TokenOnLine] -> (Maybe Bool, [String], [TokenOnLine])
parseCBOpen [{token = CBOpen}: rs] = (Just True, [], rs)
parseCBOpen [r: rs] = cantParse r "'{'" rs
parseCBOpen [] = endOfFileError

parseCBClose :: [TokenOnLine] -> (Maybe Bool, [String], [TokenOnLine])
parseCBClose [{token = CBClose}: rs] = (Just True, [], rs)
parseCBClose [r: rs] = cantParse r "'}'" rs
parseCBClose [] = endOfFileError


parseSBClose :: [TokenOnLine] -> (Maybe Bool, [String], [TokenOnLine])
parseSBClose [{token = SBClose}: rs] = (Just True, [], rs)
parseSBClose [r: rs] = cantParse r "']'" rs
parseSBClose [] = endOfFileError

parseKElse :: [TokenOnLine] -> (Maybe Bool, [String], [TokenOnLine])
parseKElse [{token = KElse}: rs] = (Just True, [], rs)
parseKElse [r: rs] = cantParse r "'else'" rs
parseKElse [] = endOfFileError

parseId :: [TokenOnLine] -> (Maybe Id, [String], [TokenOnLine])
parseId [{token = Identifier s}: rs] = (Just (PId s), [], rs)
parseId [r: rs] = cantParse r "id" rs
parseId [] = endOfFileError


isKElse :: [TokenOnLine] -> Bool
isKElse [{token = KElse}:_] = True
isKElse _ = False

isSemicolon :: [TokenOnLine] -> Bool
isSemicolon [{token = Semicolon}:_] = True
isSemicolon _ = False

isPOpen :: [TokenOnLine] -> Bool
isPOpen [{token = POpen}:_] = True
isPOpen _ = False

endOfFileError = (Nothing, ["Unexpected end of file"], [])

class cantParse a :: a String [TokenOnLine] -> (Maybe b, [String], [TokenOnLine])
instance cantParse [TokenOnLine]
	where
	cantParse [r:_] e rs = cantParse r e rs
instance cantParse TokenOnLine
	where
	cantParse {line = l} e rs = (Nothing, ["Can't parse line " +++ (toString l) +++ ", expected " +++ e], rs)

(~>) infixl 7 :: (Maybe a, [String], [TokenOnLine]) ([TokenOnLine] -> (Maybe b, [String], [TokenOnLine])) -> (Maybe a, [String], [TokenOnLine])
(~>) (t, e, rs) p2
|(isNothing t) = (Nothing, e, rs)
#(t2, e2, rs) = p2 rs
|(isNothing t2) = (Nothing, e2 ++ e, rs)
=(t, e ++ e2, rs)

(~>#) infixl 7 :: (Maybe a, [String], [TokenOnLine]) ([TokenOnLine] -> (Maybe b, [String], [TokenOnLine])) -> (Maybe (a, b), [String], [TokenOnLine])
(~>#) (t, e, rs) p2
|(isNothing t) = (Nothing, e, rs)
#(t2, e2, rs) = p2 rs
|(isNothing t2) = (Nothing, e2 ++ e, rs)
=(Just (fromJust t, fromJust t2), e2 ++ e, rs)

(~>-) infixl 7 :: (Maybe a, [String], [TokenOnLine]) ([TokenOnLine] -> (Maybe b, [String], [TokenOnLine])) -> (Maybe b, [String], [TokenOnLine])
(~>-) (t, e, rs) p2
|(isNothing t) = (Nothing, e, rs)
#(t2, e2, rs) = p2 rs
|(isNothing t2) = (Nothing, e2 ++ e, rs)
=(t2, e ++ e2, rs)

(~>.) infixl 7 :: (Maybe a, [String], [TokenOnLine]) ([TokenOnLine] -> (Maybe b, [String], [TokenOnLine])) -> (Maybe a, [String], [TokenOnLine])
(~>.) (t, e, rs) p2
#(t2, e2, rs) = p2 rs
=(t, e ++ e2, rs)
