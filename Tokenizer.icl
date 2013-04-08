implementation module Tokenizer

import StdEnv
import StdMaybe
import Result
import GenEq

:: Tokenizer :== [CharMeta] -> Maybe ([CharMeta], [Token])
:: CharMeta = { c :: Char, l :: Int, col :: Int }

symbol :: String Symbol -> Tokenizer
symbol s token = f 
where 
	sizeS = size s
	f input=:[y:ys]
	|	hasString	= Just ((drop sizeS input), [{ token = token, line = y.l, column = y.col }])
					= Nothing
	where 
		hasString = length [1 \\ x <-: s & y <- input | x == y.c] == sizeS	
		
ignore s = f
where
	f input
	|	hasString = Just (drop 1 input, [])
				  = Nothing
	where 
		hasString = length [1 \\ x <-: s | x == char.c] > 0
		char = hd input


// Accept int, only stops when digits end.
tokenizeInteger :: [CharMeta] -> Maybe ([CharMeta], [Token])
tokenizeInteger yys=:[y:ys] = if (isDigit y.c) (f yys 0) Nothing
where
	f :: [CharMeta] Int -> Maybe ([CharMeta], [Token])
	f [] i = Just ([], [{ token = Integer i, line = y.l, column = y.col }])
	f xxs=:[x:xs] i
	|	isDigit x.c = f xs ((10 * i + (digitToInt x.c)))
	=	Just (xxs, [{ token = Integer i, line = y.l, column = y.col }])
	
tokenizeId :: [CharMeta] -> Maybe ([CharMeta], [Token])
tokenizeId [y:ys] = if (isAlpha y.c || y.c == '_') (f {y.c} ys) Nothing
where
	f :: String [CharMeta] -> Maybe ([CharMeta], [Token])
	f name [] = Just ([], [{ token = Identifier name, line = y.l, column = y.col }])
	f name xxs=:[x:xs]
	|	(isAlphanum x.c || x.c == '_') = f (name +++ {x.c}) xs
	=	Just (xxs, [{ token = Identifier name, line = y.l,column = y.col }])
	
tokenizeFail :: [CharMeta] -> Maybe ([CharMeta], [Token])
tokenizeFail input=:[x:xs] = abort ("\nFailed to tokenize: \"" +++ { x.c \\ x <- (take 5 input) } +++ "\" on line " +++ (toString x.l) +++ "\n")

tokenizeComments :: [CharMeta] -> Maybe ([CharMeta], [Token])
tokenizeComments [x:xs] = if (x.c == '/') (tc xs) Nothing
where
	tc [] = Nothing
	tc [y:ys]
	|	y.c == '/' = Just (cLine ys, [])
	|	y.c == '*' = Just (cBlock ys, [])
		= Nothing
	cLine [] = []
	cLine [x:xs] = if (x.c == ("\n".[0]) || x.c == ("\r".[0])) xs (cLine xs)
	cBlock [] = []
	cBlock [x:xs] = if (x.c == '*') (cBlock2 xs) (cBlock xs)
	cBlock2 [] = []
	cBlock2 [x:xs] = if (x.c == '/') xs (cBlock xs)	


		
tokenize :: [Tokenizer] [CharMeta] -> Maybe ([CharMeta], [Token])
tokenize tokens [] = Just ([], [])
tokenize tokens input = Just (f (hd [x \\ t <- tokens, x <- [t input] | isJust x]))
where
	f :: (Maybe ([CharMeta], [Token])) -> ([CharMeta], [Token])
	f (Just (moreInput, t)) = ([], t ++ moreTokens)
	where 
		(_, moreTokens) = fromJust (tokenize tokens moreInput)	
		


tokens :: [Tokenizer]
tokens = [
		// delete Comments
		tokenizeComments,
		
		// ingore layout
		ignore " \n\r\t", // this ignores 4 characters, space, newline, return carriage and tab
	
		// Keywords		
		symbol "if" KIf, symbol "else" KElse, symbol "while" KWhile, symbol "return" KReturn,
		symbol "True" KTrue, symbol "False" KFalse,
		// Type Keywords
		symbol "Void" KVoid, symbol "Int" KInt, symbol "Bool" KBool, 
			
		// patentheses, curly brackets, square brackets
		symbol "(" POpen, symbol ")" PClose, symbol "{" CBOpen, symbol "}" CBClose, symbol "[" SBOpen, symbol "]" SBClose, 
		// interpunction
		symbol "," Comma, symbol ";" Semicolon,
	
		// OPS
		// 	Comparison
		symbol "==" (Op Eq), symbol "<=" (Op LTE),  symbol ">=" (Op GTE), symbol ">=" (Op GTE), symbol "!=" (Op NEq),
		symbol "<" (Op LT), symbol ">" (Op GT),
		// 	Logical
		symbol "&&" (Op And), symbol "||" (Op Or), symbol "!" (Op Not),	
		// 	Arithmic
		symbol "+" (Op Plus), symbol "-" (Op Min), symbol "*" (Op Mul), symbol "/" (Op Div), symbol "%" (Op Mod), 
		symbol ":" (Op Cons),
	
		// rest
		symbol "=" KAssign,
		tokenizeInteger,
		tokenizeId,
		tokenizeFail
	]
	
tokenizer :: (Result [String]) -> Result [Token]
tokenizer (Res r) = Res ((snd o fromJust o (tokenize tokens) o (toCharsInLine)) r)

toCharsInLine :: [String] -> [CharMeta]
toCharsInLine strings = f 1 strings
where
	f _ [] = []
	f i [x:xs] = [{ c = y, l = i, col= j} \\ y <-: x & j <- [0..]] ++ f (i+1) xs

derive gEq Symbol, Operator

Start = tokenizer (Res ["/*1*/{ } *\n",  "/** dont care */ 3\n", "if (foo == True)\n", "\n", "bar = 34;\n", "else bar = 302a;  // this should not show up"])

