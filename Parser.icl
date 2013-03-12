implementation module Parser

import Result
import Tokenizer

parse :: (Result [TokenOnLine]) -> Result Prog
parse {result = r}
#(p, err, y) = parseProg r
| (isNothing p) = {result = P [], errors = err}
= {result = fromJust p, errors = err}


parseProg :: [TokenOnLine] -> (Maybe Prog, [String], [TokenOnLine])
parseProg [t:tl]
#(r, e, tl) = parseRetDecl [t:tl]
|(isNothing r) = (Nothing, e, tl)
= (Just (P [F (Fun (fromJust r) (PId "temp") [] [] [])]), e, tl) // TBD choose between VarDecl or FunDecl
//parseProg [{line = l}:rs] = (Nothing, ["Can't parse line " +++ (toString l)], rs)
parseProg [] = (Nothing, [], [])

parseRetDecl :: [TokenOnLine] -> (Maybe RetType, [String], [TokenOnLine])
parseRetDecl [{token = KVoid}: rs] = (Just PVoid, [], rs)
parseRetDecl r
#(r, e, tl) = parseType r
|(isNothing r) = (Nothing, e, tl)
= (Just (RT (fromJust r)), e, tl)

parseType :: [TokenOnLine] -> (Maybe Type, [String], [TokenOnLine])
parseType [{token = KInt}: rs] = (Just TInt, [], rs)
parseType [{token = KBool}: rs] = (Just PBool, [], rs)
parseType [{token = Popen}: rs] // A tupel
#(t, e, rs) = parseType rs ~> parseComma #> parseType ~> parsePClose
|(isNothing t) = (Nothing, e, rs)
#(t1, t2) = fromJust t
= (Just (TTup (t1, t2)), e, rs)
parseType [{line = l}:rs] = (Nothing, ["Can't parse line " +++ (toString l) +++ ", expected Type"], rs)
parseType [] = endOfFileError

parseComma :: [TokenOnLine] -> (Maybe Bool, [String], [TokenOnLine])
parseComma [{token = Comma}: rs] = (Just True, [], rs)
parseComma [r: rs] = cantParse r "','" rs
parseComma [] = endOfFileError

parsePClose :: [TokenOnLine] -> (Maybe Bool, [String], [TokenOnLine])
parsePClose [{token = PClose}: rs] = (Just True, [], rs)
parsePClose [r: rs] = cantParse r "')'" rs
parsePClose [] = endOfFileError

(~>) infixl 7 :: (Maybe a, [String], [TokenOnLine]) ([TokenOnLine] -> (Maybe b, [String], [TokenOnLine])) -> (Maybe a, [String], [TokenOnLine])
(~>) (t, e, rs) p2
|(isNothing t) = (Nothing, e, rs)
#(t2, e2, rs) = p2 rs
|(isNothing t2) = (Nothing, e2 ++ e, rs)
=(t, e ++ e2, rs)

endOfFileError = (Nothing, ["Unexpected end of file"], [])
cantParse {line = l} e rs = (Nothing, ["Can't parse line " +++ (toString l) +++ ", expected " +++ e], rs)

(#>) infixl 7 :: (Maybe a, [String], [TokenOnLine]) ([TokenOnLine] -> (Maybe b, [String], [TokenOnLine])) -> (Maybe (a, b), [String], [TokenOnLine])
(#>) (t, e, rs) p2
|(isNothing t) = (Nothing, e, rs)
#(t2, e2, rs) = p2 rs
|(isNothing t2) = (Nothing, e2 ++ e, rs)
=(Just (fromJust t, fromJust t2), e2 ++ e, rs)
