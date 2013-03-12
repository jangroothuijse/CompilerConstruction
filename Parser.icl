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
#(t, e, rs) = parseFArgs rs3 ~> parsePClose ~> parseCBOpen // TBD ~># parseStmts // FunDecl
|(isNothing t) = (Nothing, e, rs) ~>. cantParse r "FunDecl"
=(Just (P [F (Fun t1 t2 (fromJust t) [] [])]), e, rs)
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
parseFArgs r
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