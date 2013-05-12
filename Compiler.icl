module Compiler

import StdEnv
import ArgEnv
import Tokenizer
import Parser
import PrettyPrinter
import SemanticAnalyzer
import SPLDefaultEnv
import IRBuilder
import SSMCodeGen
import SSMWriter

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
# prog = (parse o tokenize o toLines) file
|	size args > 2 && args.[2] == "-print"
	#console = console <<< prettyPrint console prog
	= abort ""
# defaultEnv = splDefaultEnv console
# { console = console, error = error } = analyze defaultEnv prog
| error = abort "Semantic analyzes found errors, program rejected\n"
# console = abort "Semantic analysis completed, no errors where found\n"
# (succes, outputFile, world) = fopen "a.ssm" FWriteText world
| not succes = abort "Fail to open output file"
= writeSSM outputFile ((toSSMCode o toIR) prog)
