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
parseType [{token = Popen}: rs]
#(t1, e, rs) = parseType rs
|(isNothing t1) = (Nothing, e, rs)
|(isComma (hd rs))
	#(t2, e2, rs) = parseType (tl rs)
	|(isNothing t2) = (Nothing, e ++ e2, rs)
	= (Just (TTup (fromJust t1, fromJust t2)), e ++ e2, rs)
=(Nothing, ["Can't parse line " +++ (toLineString (hd rs)) +++ ", expected ','": e], rs)

parseType [{line = l}:rs] = (Nothing, ["Can't parse line " +++ (toString l) +++ ", expected Type"], rs)
parseType [] = (Nothing, ["Unexpected end of file"], [])

isComma {token = Comma} = True
isComma _ = False

toLineString {line = l} = toString l