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
parseType [{token = Popen}: rs] = (Just (TTup (PBool, PBool)), [], rs) // TBD
parseType [{line = l}:rs] = (Nothing, ["Can't parse line " +++ (toString l)], rs)
parseType [] = (Nothing, ["Unexpected end of file"], [])
