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
|	size args > 2 && args.[2] == "-print" = prettyPrint console prog
|	size args > 2 && args.[2] == "-no-tc"
	# (succes, outputFile, world) = fopen "a.ssm" FWriteText world
	| not succes = abort "Fail to open output file"
	# ssmCode = (toSSMCode o toIR) prog
	# outputFile = writeSSM outputFile ssmCode
	#(succes, world) = fclose outputFile world
	| not succes = abort "Fail to close output file"
	= console // (ssmCode, "\n", toIR prog, "\n", prog, world)
# defaultEnv = splDefaultEnv console
# { console = console, error = error } = analyze defaultEnv prog
| error = abort "Semantic analyzes found errors, program rejected\n"
# console = console <<< "Semantic analysis completed, no errors where found\n"
# (succes, outputFile, world) = fopen "a.ssm" FWriteText world
| not succes = abort "Fail to open output file"
# ssmCode = (toSSMCode o toIR) prog
# outputFile = writeSSM outputFile ssmCode
#(succes, world) = fclose outputFile world
| not succes = abort "Fail to close output file"
= console // (ssmCode, "\n", toIR prog, "\n", prog, world)
