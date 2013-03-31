module Compiler

import StdEnv
import ArgEnv
import Tokenizer
import Parser
import PrettyPrinter
import Result
import SemanticAnalyzer
import SPLDefaultEnv

toLines :: *File -> Result [String]
toLines file
# (line, file) = freadline file
| size line == 0    =   { result = [], errors = []}
# {result = r, errors = e} = toLines file
= { result = [line: r], errors = e }

//Start :: *World -> String
Start world
#   args = getCommandLine
|   size args < 2 = abort "\nNo filename supplied (missing argument)\n"
#   filename = args.[1]
#   (succes, file, world)   = fopen filename FReadText world
|   not succes  =   abort ("\nUnable to open " +++ filename +++ "\n")
# tok = (tokenizer (toLines file))
# ast = parse tok
|(isNothing ast.result) = (tok, prettyPrint tok, ast, "", "")
=   (tok, prettyPrint tok, ast, pretty 0 (fromJust ast.result), foldl (+++) "" (map ((+++) "\n") (analyze splDefaultEnv (fromJust ast.result)).envErrors))

