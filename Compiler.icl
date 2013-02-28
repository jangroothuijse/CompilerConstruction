module Compiler

import StdEnv
import ArgEnv
import Tokenizer

toLines :: *File -> [String]
toLines file
# (line, file) = freadline file
| size line == 0	=	[]
= [line: toLines file]

Start :: *World -> [TokenOnLine]
Start world
#	args = getCommandLine
|	size args < 2 = abort "\nNo filename supplied (missing argument)\n"
#	filename = args.[1]
#	(succes, file, world)	= fopen filename FReadText world
|	not succes	=	abort ("\nUnable to open " +++ filename +++ "\n")
=	tokenizer (toLines file)
