module Compiler

import StdEnv
import ArgEnv
import Tokenizer
import Parser
import PrettyPrinter
import Result
import SemanticAnalyzer
import CompilerTest

toLines :: *File -> Result [String]
toLines file
# (line, file) = freadline file
| size line == 0    =   Res []
# (Res r) = toLines file
= Res [line: r]

//Start :: *World -> String
Start world
#   args = getCommandLine
|   size args < 2 = abort "\nNo filename supplied (missing argument)\n"
#   filename = args.[1]
#   (succes, file, world)   = fopen filename FReadText world
|   not succes  =   abort ("\nUnable to open " +++ filename +++ "\n")
# tok = (tokenizer (toLines file))
# ast = parse tok
=case ast of Err e = (tok, prettyPrint tok, ast, "", Err [""])
			 Res r = (tok, prettyPrint tok, ast, pretty 0 r, staticAnalyze ast)


