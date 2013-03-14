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
|(isNothing t1) = (Just [fromJust t], e1 ++ e, rs) // return successfull tree TBD: Skip failed part and parse next function
=(Just [fromJust t: fromJust t1], e1 ++ e, rs)

parseDecl :: [TokenOnLine] -> (Maybe Decl, [String], [TokenOnLine])
parseDecl r=:[_:_]
#(t, e, rs) = parseRetDecl r ~># parseId
|(isNothing t) = (Nothing, e, rs) ~>. cantParse r "VarDecl or FunDecl"
#(t1, t2) = fromJust t
#(t, e1, rs1) = parsePopen rs
|(isNothing t) // VarDecl
	|(isPVoid t1) = cantParse r "FunDecl" rs1
	#(t3, e2, rs) = parseKAssign rs ~>#- ParseExp ~> ParseSemicolon
	#(RT t1) = t1
	= (Just (V (VD t1 t2 (fromJust t3))), e2 ++ e1 ++ e, rs)
#(t, e2, rs) = parseFArgs_ rs1 ~> parsePClose ~> parseCBOpen ~># parseVarDecls_ ~># parseStmts // FunDecl
|(isNothing t) = (Nothing, e2 ++ e1 ++ e, rs) ~>. cantParse r "FunDecl"
#((t3, t4), t5) = fromJust t
=(Just (F (Fun t1 t2 t3 t4 t5)), e, rs)
parseDecl [] = (Nothing, [], [])

parseRetDecl :: [TokenOnLine] -> (Maybe RetType, [String], [TokenOnLine])
parseRetDecl [{token = KVoid}: rs] = (Just PVoid, [], rs)
parseRetDecl r
#(t, e, rs) = parseType r
|(isNothing t) = (Nothing, e, rs)
=(Just (RT (fromJust t)), e, rs)

isPVoid :: RetType -> Bool
isPVoid PVoid = True
isPVoid _ = False

parseType :: [TokenOnLine] -> (Maybe Type, [String], [TokenOnLine])
parseType [{token = KInt}: rs] = (Just TInt, [], rs)
parseType [{token = KBool}: rs] = (Just PBool, [], rs)
parseType [{token = Popen}: rs] // A tupel
#(t, e, rs) = parseType rs ~> parseComma ~># parseType ~> parsePClose
|(isNothing t) = (Nothing, e, rs)
#(t1, t2) = fromJust t
=(Just (TTup (t1, t2)), e, rs)
parseType r=:[{token = Identifier _}: rs]
#(t, e, rs) = parseId r
=(Just PBool, [], rs)
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
#(t, e, rs) = parseType r ~># parseId ~> parseKAssign ~># ParseExp ~> ParseSemicolon
|(isNothing t) =  (Nothing, e, rs) ~>. cantParse r "VarDecl"
= (Nothing, [], r)

ParseExp :: [TokenOnLine] -> (Maybe Exp, [String], [TokenOnLine])
ParseExp r // TBD
= (Nothing, [], r)

parseStmts :: [TokenOnLine] -> (Maybe [Stmt], [String], [TokenOnLine])
parseStmts r // Stmt+
#(t, e, rs) = parseStmt r
|(isNothing t) = (Nothing, e, rs)
#(t1, e1, rs) = parseStmts rs
|(isNothing t1) = (Just [fromJust t], e, rs)
=(Just [fromJust t: fromJust t1], e, rs)

parseStmt :: [TokenOnLine] -> (Maybe Stmt, [String], [TokenOnLine])
parseStmt [{token = CBOpen}:rs] // Block
#(t, e, rs1) = parsePClose rs
|(isNothing t) // Block with statements
	#(t, e, rs) = parseStmts rs
	|(isNothing t) = (Nothing, e, rs)
	=(Just (Block (fromJust t)), e, rs)
=(Just (Block []), e, rs1) // Empty block
parseStmt [{token = CBClose}:rs] // TBD
=(Nothing, [], rs)
parseStmt [_:rs] // TBD
=(Just Return, [], rs)
parseStmt [] = endOfFileError

ParseSemicolon :: [TokenOnLine] -> (Maybe Bool, [String], [TokenOnLine])
ParseSemicolon [{token = Semicolon}: rs] = (Just True, [], rs)
ParseSemicolon [r: rs] = cantParse r "';'" rs
ParseSemicolon [] = endOfFileError

parseKAssign :: [TokenOnLine] -> (Maybe Bool, [String], [TokenOnLine])
parseKAssign [{token = KAssign}: rs] = (Just True, [], rs)
parseKAssign [r: rs] = cantParse r "'='" rs
parseKAssign [] = endOfFileError

parseComma :: [TokenOnLine] -> (Maybe Bool, [String], [TokenOnLine])
parseComma [{token = Comma}: rs] = (Just True, [], rs)
parseComma [r: rs] = cantParse r "','" rs
parseComma [] = endOfFileError

parsePopen :: [TokenOnLine] -> (Maybe Bool, [String], [TokenOnLine])
parsePopen [{token = Popen}: rs] = (Just True, [], rs)
parsePopen [r: rs] = cantParse r "'('" rs
parsePopen [] = endOfFileError

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

parseId :: [TokenOnLine] -> (Maybe Id, [String], [TokenOnLine])
parseId [{token = Identifier s}: rs] = (Just (PId s), [], rs)
parseId [r: rs] = cantParse r "id" rs
parseId [] = endOfFileError

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

(~>#-) infixl 7 :: (Maybe a, [String], [TokenOnLine]) ([TokenOnLine] -> (Maybe b, [String], [TokenOnLine])) -> (Maybe b, [String], [TokenOnLine])
(~>#-) (t, e, rs) p2
|(isNothing t) = (Nothing, e, rs)
#(t2, e2, rs) = p2 rs
|(isNothing t2) = (Nothing, e2 ++ e, rs)
=(t2, e ++ e2, rs)

(~>.) infixl 7 :: (Maybe a, [String], [TokenOnLine]) ([TokenOnLine] -> (Maybe b, [String], [TokenOnLine])) -> (Maybe a, [String], [TokenOnLine])
(~>.) (t, e, rs) p2
#(t2, e2, rs) = p2 rs
=(t, e ++ e2, rs)