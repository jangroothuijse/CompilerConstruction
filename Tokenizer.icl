implementation module Tokenizer

import StdEnv
import StdMaybe
import Result

:: Tokenizer :== [CharOnLine] -> Maybe ([CharOnLine], [TokenOnLine])
:: CharOnLine = { c :: Char, l :: Int }

symbol :: String Token -> Tokenizer
symbol s token = f 
where 
	sizeS = size s
	f input=:[y:ys]
	|	hasString	= Just ((drop sizeS input), [{ token = token, line = y.l }])
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
tokenizeInteger :: [CharOnLine] -> Maybe ([CharOnLine], [TokenOnLine])
tokenizeInteger yys=:[y:ys] = if (isDigit y.c) (f yys 0) Nothing
where
	f :: [CharOnLine] Int -> Maybe ([CharOnLine], [TokenOnLine])
	f [] i = Just ([], [{ token = Integer i, line = y.l }])
	f xxs=:[x:xs] i
	|	isDigit x.c = f xs (10 * (i + (digitToInt x.c)))
	=	Just (xxs, [{ token = Integer i, line = y.l }])
	
tokenizeId :: [CharOnLine] -> Maybe ([CharOnLine], [TokenOnLine])
tokenizeId [y:ys] = if (isAlpha y.c) (f {y.c} ys) Nothing
where
	f :: String [CharOnLine] -> Maybe ([CharOnLine], [TokenOnLine])
	f name [] = Just ([], [{ token = Identifier name, line = y.l }])
	f name xxs=:[x:xs]
	|	isAlphanum x.c = f (name +++ {x.c}) xs
	=	Just (xxs, [{ token = Identifier name, line = y.l }])
	
tokenizeFail :: [CharOnLine] -> Maybe ([CharOnLine], [TokenOnLine])
tokenizeFail input=:[x:xs] = abort ("\nFailed to tokenize: \"" +++ { x.c \\ x <- (take 5 input) } +++ "\" on line " +++ (toString x.l) +++ "\n")

tokenizeComments :: [CharOnLine] -> Maybe ([CharOnLine], [TokenOnLine])
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


		
tokenize :: [Tokenizer] [CharOnLine] -> Maybe ([CharOnLine], [TokenOnLine])
tokenize tokens [] = Just ([], [])
tokenize tokens input = Just (f (hd [x \\ t <- tokens, x <- [t input] | isJust x]))
where
	f :: (Maybe ([CharOnLine], [TokenOnLine])) -> ([CharOnLine], [TokenOnLine])
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
		symbol "&&" (Op And), symbol "||" (Op Or),		
		// 	Arithmic
		symbol "+" (Op Plus), symbol "-" (Op Min), symbol "*" (Op Mul), symbol "/" (Op Div), symbol "%" (Op Mod), 
		symbol ":" (Op Cons),
	
		// rest
		symbol "=" KAssign,
		tokenizeInteger,
		tokenizeId,
		tokenizeFail
	]
	
tokenizer :: (Result [String]) -> Result [TokenOnLine]
tokenizer {result = r} = {result = (snd o fromJust o (tokenize tokens) o (toCharsInLine)) r, errors = []}

toCharsInLine :: [String] -> [CharOnLine]
toCharsInLine strings = f 1 strings
where
	f _ [] = []
	f i [x:xs] = [{ c = y, l = i} \\ y <-: x] ++ f (i+1) xs

Start = tokenizer {result = ["/*1*/{ } *\n",  "/** dont care */ 3\n", "if (foo == True)\n", "\n", "bar = 34;\n", "else bar = 302a;  // this should not show up"], errors = []}
