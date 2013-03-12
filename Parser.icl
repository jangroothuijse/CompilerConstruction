implementation module Parser

import Result
import Tokenizer

parse :: (Result [TokenOnLine]) -> Result Prog
parse {result = r}
#(p, err, y) = parseProg r
|(isNothing p) = {result = P [], errors = err}
={result = fromJust p, errors = err}


parseProg :: [TokenOnLine] -> (Maybe Prog, [String], [TokenOnLine])
parseProg [r:rs]
#(t, e, rs) = parseRetDecl [r:rs] ~># parseId
|(isNothing t) = (Nothing, e, rs) ~>. cantParse r "VarDecl or FunDecl"
#(t1, t2) = fromJust t
#(t3, e3, rs3) = parsePopen rs
|(isNothing t3) = cantParse r "VarDecl unsupported" rs // TBD
#(t, e, rs) = parseFArgs rs3 ~> parsePClose ~> parseCBOpen ~># parseVarDecls_ ~># parseStmts // FunDecl
|(isNothing t) = (Nothing, e, rs) ~>. cantParse r "FunDecl"
#((t4, t5), t6) = fromJust t
=(Just (P [F (Fun t1 t2 t4 t5 t6)]), e, rs)
//=cantParse r "VarDecl or FunDecl"
parseProg [] = (Nothing, [], [])

parseRetDecl :: [TokenOnLine] -> (Maybe RetType, [String], [TokenOnLine])
parseRetDecl [{token = KVoid}: rs] = (Just PVoid, [], rs)
parseRetDecl r
#(t, e, rs) = parseType r
|(isNothing t) = (Nothing, e, rs)
=(Just (RT (fromJust t)), e, rs)

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
parseType [r:rs] = cantParse r "Type" rs
parseType [] = endOfFileError

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
|(isNothing t) =  (Nothing, e, rs) ~>. cantParse (hd r) "VarDecl"
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

parseId :: [TokenOnLine] -> (Maybe Id, [String], [TokenOnLine])
parseId [{token = Identifier s}: rs] = (Just (PId s), [], rs)
parseId [r: rs] = cantParse r "id" rs
parseId [] = endOfFileError


endOfFileError = (Nothing, ["Unexpected end of file"], [])
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


(~>.) infixl 7 :: (Maybe a, [String], [TokenOnLine]) ([TokenOnLine] -> (Maybe b, [String], [TokenOnLine])) -> (Maybe a, [String], [TokenOnLine])
(~>.) (t, e, rs) p2
#(t2, e2, rs) = p2 rs
=(t, e ++ e2, rs)