implementation module Tokenizer

import StdEnv, StdMaybe, GenEq

:: Tokenizer :== [CharMeta] -> Maybe ([CharMeta], [Token])
:: CharMeta = { c :: Char, l :: Int, col :: Int }

tokenizer :: [CharMeta] -> [Token]

tokenizer [] = []

tokenizer [{c = '/'}:[{c = '*'}:xs]] = tokenizerCommentBlock xs
tokenizer [{c = '/'}:[{c = '/'}:xs]] = tokenizerCommentLine xs

tokenizer [{c = ' ', l = l, col = col}:xs] = tokenizer xs
tokenizer [{c = '\t', l = l, col = col}:xs] = tokenizer xs
tokenizer [{c = '\n', l = l, col = col}:xs] = tokenizer xs
tokenizer [{c = '\r', l = l, col = col}:xs] = tokenizer xs

tokenizer [cm=:{c = '('}:xs] = [t cm POpen : tokenizer xs]
tokenizer [cm=:{c = ')'}:xs] = [t cm PClose: tokenizer xs]
tokenizer [cm=:{c = '{'}:xs] = [t cm CBOpen: tokenizer xs]
tokenizer [cm=:{c = '}'}:xs] = [t cm CBClose: tokenizer xs]
tokenizer [cm=:{c = '['}:xs] = [t cm SBOpen: tokenizer xs]
tokenizer [cm=:{c = ']'}:xs] = [t cm SBClose: tokenizer xs]

tokenizer [cm=:{c = ','}:xs] = [t cm Comma: tokenizer xs]
tokenizer [cm=:{c = ';'}:xs] = [t cm Semicolon: tokenizer xs]

tokenizer [cm=:{c = '&'}:[{c = '&'}:xs]] = [t cm (Op And): tokenizer xs]
tokenizer [cm=:{c = '|'}:[{c = '|'}:xs]] = [t cm (Op Or): tokenizer xs]
tokenizer [cm=:{c = '|'}:xs] = [t cm Bar: tokenizer xs]

tokenizer [cm=:{c = '='}:[{c = '='}:xs]] = [t cm (Op Eq): tokenizer xs]
tokenizer [cm=:{c = '<'}:[{c = '='}:xs]] = [t cm (Op LTE): tokenizer xs]
tokenizer [cm=:{c = '>'}:[{c = '='}:xs]] = [t cm (Op GTE): tokenizer xs]
tokenizer [cm=:{c = '!'}:[{c = '='}:xs]] = [t cm (Op NEq): tokenizer xs]
tokenizer [cm=:{c = '<'}:xs] = [t cm (Op LT): tokenizer xs]
tokenizer [cm=:{c = '>'}:xs] = [t cm (Op GT): tokenizer xs]
tokenizer [cm=:{c = '='}:xs] = [t cm KAssign: tokenizer xs]

tokenizer [cm=:{c = '+'}:xs] = [t cm (Op Plus): tokenizer xs]
tokenizer [cm=:{c = '-'}:xs] = [t cm (Op Min): tokenizer xs]
tokenizer [cm=:{c = '*'}:xs] = [t cm (Op Mul): tokenizer xs]
tokenizer [cm=:{c = '%'}:xs] = [t cm (Op Mod): tokenizer xs]
tokenizer [cm=:{c = '/'}:xs] = [t cm (Op Div): tokenizer xs]
tokenizer [cm=:{c = ':'}:xs] = [t cm (Op Cons): tokenizer xs]
tokenizer [cm=:{c = '!'}:xs] = [t cm (Op Not): tokenizer xs]

tokenizer [x:xs]
| isAlpha x.c || x.c == '_'	= tokenizerString [x.c] xs x
| isDigit x.c				= tokenizerInt (digitToInt x.c) xs x
= abort ("Cannot tokenize" +++ (toString x.c))

tokenizerString :: [Char] [CharMeta] CharMeta -> [Token]
tokenizerString acc xxs=:[x:xs] begin
| isAlphanum x.c || x.c == '_' = tokenizerString (acc ++ [x.c]) xs begin
# name = { c \\ c <- acc } // toString conversion
= case name of 
	"if"		= [t begin KIf:tokenizer xxs]
	"else"		= [t begin KElse:tokenizer xxs]
	"while"		= [t begin KWhile:tokenizer xxs]
	"return"	= [t begin KReturn:tokenizer xxs]
	"Bool"		= [t begin KBool:tokenizer xxs]
	"Int"		= [t begin KInt:tokenizer xxs]
	"True"		= [t begin KTrue:tokenizer xxs]
	"False"		= [t begin KFalse:tokenizer xxs]
	"Void"		= [t begin KVoid:tokenizer xxs]
	"type"		= [t begin KType:tokenizer xxs]
	"match"		= [t begin KMatch:tokenizer xxs]
	"case"		= [t begin KCase:tokenizer xxs]
	s			= [t begin (Identifier s):tokenizer xxs]

tokenizerInt :: !Int [CharMeta] CharMeta -> [Token]
tokenizerInt i xxs=:[x:xs] begin
|	isDigit x.c		= tokenizerInt ((10 * i) + (digitToInt x.c)) xs begin
=	[t begin (Integer i): tokenizer xxs]

tokenizerCommentLine [{c = '\n'}:xs] = tokenizer xs
tokenizerCommentLine [x:xs] = tokenizerCommentLine xs

tokenizerCommentBlock [{c = '*'}:[{c = '/'}:xs]] = tokenizer xs
tokenizerCommentBlock [x:xs] = tokenizerCommentBlock xs

t :: CharMeta Symbol -> Token
t cm s = { Token | token = s, line = cm.CharMeta.l, column = cm.CharMeta.col }

tokenize :: ([String] -> [Token])
tokenize = tokenizer o (toCharsInLine)

toCharsInLine :: [String] -> [CharMeta]
toCharsInLine strings = f 1 0 strings
where
	f :: !Int !Int [String] -> [CharMeta]
	f _ _ [] = []
	f i j l=:[x:xs]
	|	j < size x	= [{ c = x.[j], l = i, col = j+1}: f i (j + 1) l]
					= f (i + 1) 0 xs

derive gEq Symbol, Operator

Start = tokenize ["/*1*/{ } *\n",  "/** dont care */ 3\n", "if (foo == True)\n", "\n", "bar = 34;\n", "else bar = 302a;  // this should not show up"]

