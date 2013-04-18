module Compiler

import StdEnv
import ArgEnv
import Tokenizer
import Parser
import PrettyPrinter
import Result
import SemanticAnalyzer
import SPLDefaultEnv

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
|	size args > 2 && args.[2] == "-print" = prettyPrint console (parse (tokenize (toLines file)))
# defaultEnv = splDefaultEnv console
 = (analyze defaultEnv (parse (tokenize (toLines file)))).console
//= u.console
//# (checked, u) = (check (parse (tokenize (toLines file))) defaultEnv)
//# console = u.UEnv.console
//# error = u.UEnv.error
//# console = console <<< ((toString (length checked)) +++ " definitions\n")
//| error = console <<< "Semantic errors where found, program rejected\n"
//# console = console <<< "Semantic analysis completed, no errors where found"
//= console
				


