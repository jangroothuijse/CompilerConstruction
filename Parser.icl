implementation module Parser

import Result
import Tokenizer

parse :: (Result [Token]) -> Result (Maybe Prog)
parse {result = r}
#(p, err, y) = parseProg r
|(isNothing p) = {result = Nothing, errors = err}
={result = p, errors = err}

parseProg :: [Token] -> (Maybe Prog, [String], [Token])
parseProg r=:[_:_]
#(t, e, rs) = parseDecls r
|(isNothing t) = (Nothing, e, rs)
= (Just (P (fromJust t)), e, rs)
parseProg [] = endOfFileError

parseDecls :: [Token] -> (Maybe [Decl], [String], [Token])
parseDecls r // Decl+
#(t, e, rs) = parseDecl r
|(isNothing t) = (Nothing, e, rs)
|(isEmpty rs) = (Just [fromJust t], e, rs) // Last Decl reached
#(t1, e1, rs) = parseDecls rs
|(isNothing t1) = (Just [fromJust t], e1 ++ e, rs) // return successfull tree // TBD: Skip failed part and parse next function
=(Just [fromJust t: fromJust t1], e1 ++ e, rs)

parseDecl :: [Token] -> (Maybe Decl, [String], [Token])
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


parseRetDecl :: [Token] -> (Maybe RetType, [String], [Token])
parseRetDecl [{token = KVoid}: rs] = (Just PVoid, [], rs)
parseRetDecl r
#(t, e, rs) = parseType r
|(isNothing t) = (Nothing, e, rs)
=(Just (RT (fromJust t)), e, rs)

parseType :: [Token] -> (Maybe Type, [String], [Token])
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

parseFArgs_ :: [Token] -> (Maybe [FArgs], [String], [Token])
parseFArgs_ r=:[{token = PClose}: _] = (Just [], [], r) // FArgs*
parseFArgs_ r = parseFArgs r

parseFArgs :: [Token] -> (Maybe [FArgs], [String], [Token])
parseFArgs r // FArgs+
#(t, e, rs) = parseFArg r
|(isNothing t) = (Nothing, e, rs)
#(t1, e1, rs1) = parseComma rs
|(isNothing t1) = (Just [fromJust t], e, rs) // Last FArgs reached
#(t1, e2, rs) = parseFArgs rs1
|(isNothing t1)= (Nothing, e2 ++ e1 ++ e, rs)
=(Just [fromJust t:fromJust t1], e2 ++ e1 ++ e, rs)

parseFArg :: [Token] -> (Maybe FArgs, [String], [Token])
parseFArg r
#(t, e, rs) = parseType r ~># parseId
|(isNothing t) = (Nothing, e, rs)
#(t1, t2) = fromJust t
=(Just (FA t1 t2), e, rs)

parseVarDecls_ :: [Token] -> (Maybe [VarDecl], [String], [Token])
parseVarDecls_ r // VarDecl*
#(t, e, rs) = parseVarDecl r
|(isNothing t) = (Just [], [], r)// At this point backtracing is easier
#(t1, e1, rs) = parseVarDecls_ rs
=(Just [fromJust t:fromJust t1], e1 ++ e, rs)

parseVarDecl :: [Token] -> (Maybe VarDecl, [String], [Token])
parseVarDecl r
#(t, e, rs) = parseType r ~># parseId ~> parseKAssign ~># parseExp ~> parseSemicolon
|(isNothing t) =  (Nothing, e, rs) ~>. cantParse r "VarDecl"
#((t1, t2), t3) = fromJust t
= (Just (VD t1 t2 t3), e, rs)

parseStmts :: [Token] -> (Maybe [Stmt], [String], [Token])
parseStmts r // Stmt+
#(t, e, rs) = parseStmt r
|(isNothing t) = (Nothing, e, rs)
#(t1, e1, rs1) = parseStmts rs
|(isNothing t1) = (Just [fromJust t], e, rs)
=(Just [fromJust t: fromJust t1], e1 ++ e, rs1)

parseStmt :: [Token] -> (Maybe Stmt, [String], [Token])
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

parseActArgs_ :: [Token] -> (Maybe [ActArgs], [String], [Token])
parseActArgs_ r=:[{token = PClose}: _] = (Just [], [], r) // ActArgs*
parseActArgs_ r = parseActArgs r

parseActArgs :: [Token] -> (Maybe [ActArgs], [String], [Token])
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
 
parseExp :: ([Token] -> (Maybe Exp, [String], [Token]))
parseExp = parseConsExp

parseConsExp :: [Token] -> (Maybe Exp, [String], [Token])
parseConsExp rs
#	(t, e, rs)		= parseAndExp rs
|	not (isJust t) 	= (Nothing, e, rs) ~>. cantParse rs "ConsExpression"
=	case rs of
	[{token = Op Cons}:rs] = if (isJust t2) (Just (Op2 (fromJust t) PCons (fromJust t2)), e2 ++ e, rrs) (Nothing, e2 ++ e, rrs)
	where (t2, e2, rrs) = parseConsExp rs
	_					= (t, e, rs)

parseAndExp :: [Token] -> (Maybe Exp, [String], [Token])
parseAndExp rs
#	(t, e, rs) 		= parseOrExp rs
|	isNothing t		= (Nothing, e, rs)
=	case rs of
	[{token = Op And}:rrs]	= if (isJust t2) (Just (Op2 (fromJust t) PAnd (fromJust t2)), e2 ++ e, rs2) (Nothing, e, rs)	
	where (t2, e2, rs2) = parseAndExp rrs
	_				= (t, e, rs)
	
parseOrExp :: [Token] -> (Maybe Exp, [String], [Token])
parseOrExp rs
#	(t, e, rs) 		= parseRelExp rs
|	isNothing t		= (Nothing, e, rs)
=	case rs of
	[{token = Op Or}:rrs]	= if (isJust t2) (Just (Op2 (fromJust t) POr (fromJust t2)), e2 ++ e, rs2) (Nothing, e, rs)	
	where (t2, e2, rs2) = parseOrExp rrs
	_				= (t, e, rs)

parseRelExp :: [Token] -> (Maybe Exp, [String], [Token])
parseRelExp rs
#	(t, e, rs) 		= parseSum rs
|	not (isJust t) 	= (Nothing, e, rs) ~>. cantParse rs "RelExpression"
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
		emitOperator :: Op2 -> (Maybe Exp, [String], [Token])
		emitOperator operator = if (isJust t2) (Just (Op2 exp1 operator (fromJust t2)), e2 ++ e, rs2) (Nothing, e, rs)	
	_	=	(t, e, rs)

parseSum :: [Token] -> (Maybe Exp, [String], [Token])
parseSum rs
#	(t, e, rs)		= parseTerm rs
|	not (isJust t)	= (Nothing, e, rs) ~>. cantParse rs "SumExpression"
#	term1			= fromJust t
=	case rs of
	[{token = Op op}:rrs] = case op of
		Plus	= emitOperator PPlus
		Min		= emitOperator PMin
		_ 		= (t, e, rs)
	where
		(t2, e2, rs2) = parseSum rrs
		emitOperator :: Op2 -> (Maybe Exp, [String], [Token])
		emitOperator operator = if (isJust t2) (Just (Op2 term1 operator (fromJust t2)), e2 ++ e, rs2) (Nothing, e, rs)	
	_	=	(t, e, rs)	

parseTerm :: [Token] -> (Maybe Exp, [String], [Token])
parseTerm rs
#	(t, e, rs)		= parseFactor rs
|	not (isJust t)	= (Nothing, e, rs) ~>. cantParse rs "Term"
#	fact1			= fromJust t
=	case rs of
	[{token = Op op}:rrs] = case op of
		Mul		= emitOperator PMul
		Div		= emitOperator PDiv
		Mod		= emitOperator PMod
		_ 		= (t, e, rs)
	where
		(t2, e2, rs2) = parseTerm rrs
		emitOperator :: Op2 -> (Maybe Exp, [String], [Token])
		emitOperator operator = if (isJust t2) (Just (Op2 fact1 operator (fromJust t2)), e2 ++ e, rs2) (Nothing, e, rs)	
	_	=	(t, e, rs)	
	
parseFactor :: [Token] -> (Maybe Exp, [String], [Token])
parseFactor [{token = Op Not}:rs]
#	(t, e, rs)		= parseFactor rs
|	isJust t		= (Just (Op1 PNot (fromJust t)), e, rs)
					= (Nothing, e, rs) ~>. cantParse rs "Factor"
parseFactor [{token = Op Min}:rs]
#	(t, e, rs)		= parseFactor rs
|	isJust t		= (Just (Op1 PNeg (fromJust t)), e, rs)
					= (Nothing, e, rs) ~>. cantParse rs "Factor"
parseFactor [{token = Integer z}:rs] = (Just (EInt z), [], rs)
parseFactor [{token = KTrue}:rs] = (Just ETrue, [], rs)
parseFactor [{token = KFalse}:rs] = (Just ETrue, [], rs)
parseFactor [{token = POpen}:rs] // Parentheses AND Tuples...
# 	(t, e, rs)		= parseExp rs
|	isJust t		= case rs of 
	[{token = Comma}:rrs] = case r.token of
		PClose	= if (isJust t2) (Just (Tup (fromJust t) (fromJust t2)), e2 ++ e, rrrs) (Nothing, e2 ++ e, rrrs)
		x = (Nothing, e2 ++ e, rs) ~>. cantParse rs ")"
	where (t2, e2, [r:rrrs]) = parseExp rrs
	[{token = PClose}:rrs] 	= (Just (EBrace (fromJust t)), e, rrs)
	_						= (Nothing, e, rs) ~>. cantParse rs ")"
=	(t, e, rs)
parseFactor [{token = SBOpen}:rs] = case rs of 
	[{token = SBClose}: rrs]	= (Just EBlock, [], rrs)
	x							= cantParse rs "[]" rs
parseFactor x = parseIdAndCall x

parseIdAndCall :: [Token] -> (Maybe Exp, [String], [Token])
parseIdAndCall [{token = Identifier name}:rs] = case rs of
	[{token = POpen}:rrs]
	#(t, e,rs) = parsePOpen rs ~>- parseActArgs_ ~> parsePClose
	|(isNothing t) = (Nothing, e, rs)
	= (Just (EFC (FC (PId name) (fromJust t))), e, rs)
	_						= (Just (I (PId name)), [], rs)		// ID
parseIdAndCall x = cantParse x "Id or Call" x
 
parseSemicolon :: [Token] -> (Maybe Bool, [String], [Token])
parseSemicolon [{token = Semicolon}: rs] = (Just True, [], rs)
parseSemicolon [r: rs] = cantParse r "';'" rs
parseSemicolon [] = endOfFileError

parseKAssign :: [Token] -> (Maybe Bool, [String], [Token])
parseKAssign [{token = KAssign}: rs] = (Just True, [], rs)
parseKAssign [r: rs] = cantParse r "'='" rs
parseKAssign [] = endOfFileError

parseComma :: [Token] -> (Maybe Bool, [String], [Token])
parseComma [{token = Comma}: rs] = (Just True, [], rs)
parseComma [r: rs] = cantParse r "','" rs
parseComma [] = endOfFileError

parsePOpen :: [Token] -> (Maybe Bool, [String], [Token])
parsePOpen [{token = POpen}: rs] = (Just True, [], rs)
parsePOpen [r: rs] = cantParse r "'('" rs
parsePOpen [] = endOfFileError

parsePClose :: [Token] -> (Maybe Bool, [String], [Token])
parsePClose [{token = PClose}: rs] = (Just True, [], rs)
parsePClose [r: rs] = cantParse r "')'" rs
parsePClose [] = endOfFileError

parseCBOpen :: [Token] -> (Maybe Bool, [String], [Token])
parseCBOpen [{token = CBOpen}: rs] = (Just True, [], rs)
parseCBOpen [r: rs] = cantParse r "'{'" rs
parseCBOpen [] = endOfFileError

parseCBClose :: [Token] -> (Maybe Bool, [String], [Token])
parseCBClose [{token = CBClose}: rs] = (Just True, [], rs)
parseCBClose [r: rs] = cantParse r "'}'" rs
parseCBClose [] = endOfFileError


parseSBClose :: [Token] -> (Maybe Bool, [String], [Token])
parseSBClose [{token = SBClose}: rs] = (Just True, [], rs)
parseSBClose [r: rs] = cantParse r "']'" rs
parseSBClose [] = endOfFileError

parseKElse :: [Token] -> (Maybe Bool, [String], [Token])
parseKElse [{token = KElse}: rs] = (Just True, [], rs)
parseKElse [r: rs] = cantParse r "'else'" rs
parseKElse [] = endOfFileError

parseId :: [Token] -> (Maybe Id, [String], [Token])
parseId [{token = Identifier s}: rs] = (Just (PId s), [], rs)
parseId [r: rs] = cantParse r "id" rs
parseId [] = endOfFileError


isKElse :: [Token] -> Bool
isKElse [{token = KElse}:_] = True
isKElse _ = False

isSemicolon :: [Token] -> Bool
isSemicolon [{token = Semicolon}:_] = True
isSemicolon _ = False

isPOpen :: [Token] -> Bool
isPOpen [{token = POpen}:_] = True
isPOpen _ = False

endOfFileError = ParseError "Unexpected end of file" []

class cantParse a :: a String [Token] -> (Maybe b, [String], [Token])
instance cantParse [Token]
	where
	cantParse [r:_] e rs = cantParse r e rs
	cantParse _ e rs = ParseError ("Expected " +++ e) rs
instance cantParse Token
	where
	cantParse r e rs = ParseErrorLine r ("Expected " +++ e) rs

ParseErrorLine {line = l, column = c} e rs = ParseError ("Line " +++ (toString l) +++ ":" +++ (toString c) +++ ". " +++ e) rs
ParseError e rs = (Nothing, [e], rs)

(~>) infixl 7 :: (Maybe a, [String], [Token]) ([Token] -> (Maybe b, [String], [Token])) -> (Maybe a, [String], [Token])
(~>) (t, e, rs) p2
|(isNothing t) = (Nothing, e, rs)
#(t2, e2, rs) = p2 rs
|(isNothing t2) = (Nothing, e2 ++ e, rs)
=(t, e2 ++ e, rs)

(~>#) infixl 7 :: (Maybe a, [String], [Token]) ([Token] -> (Maybe b, [String], [Token])) -> (Maybe (a, b), [String], [Token])
(~>#) (t, e, rs) p2
|(isNothing t) = (Nothing, e, rs)
#(t2, e2, rs) = p2 rs
|(isNothing t2) = (Nothing, e2 ++ e, rs)
=(Just (fromJust t, fromJust t2), e2 ++ e, rs)

(~>-) infixl 7 :: (Maybe a, [String], [Token]) ([Token] -> (Maybe b, [String], [Token])) -> (Maybe b, [String], [Token])
(~>-) (t, e, rs) p2
|(isNothing t) = (Nothing, e, rs)
#(t2, e2, rs) = p2 rs
|(isNothing t2) = (Nothing, e2 ++ e, rs)
=(t2, e2 ++ e, rs)

(~>.) infixl 7 :: (Maybe a, [String], [Token]) ([Token] -> (Maybe b, [String], [Token])) -> (Maybe a, [String], [Token])
(~>.) (t, e, rs) p2
#(t2, e2, rs) = p2 rs
=(t, e2 ++ e, rs)
