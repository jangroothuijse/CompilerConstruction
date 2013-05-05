module Compiler

import StdEnv
import ArgEnv
import Tokenizer
import Parser
import PrettyPrinter
import SemanticAnalyzer
import SPLDefaultEnv
import IRBuilder

toLines :: *File -> [String]
toLines file
# (line, file) = freadline file
| size line == 0 = []
= [line: toLines file]

Start world
# 	(console, world) = stdio world
#   args = getCommandLine
|   size args < 2 = abort "\nNo filename supplied (missing argument)\n"
#   filename = args.[1]
#   (succes, file, world)   = fopen filename FReadText world
|   not succes  =   abort ("\nUnable to open " +++ filename +++ "\n")
# console = console <<< ("Compiling " +++ filename +++ "\n")
|	size args > 2 && args.[2] == "-print" = prettyPrint console ((parse o tokenize o toLines) file)

# defaultEnv = splDefaultEnv console
# { console = console, error = error } = analyze defaultEnv ((parse o tokenize o toLines) file)
| error = console <<< "Semantic analyzes found errors, program rejected\n"
= console <<< "Semantic analysis completed, no errors where found\n"

