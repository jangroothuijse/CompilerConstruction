definition module Tokenizer

import GenEq

:: Token = { token :: Symbol, line :: Int, column :: Int }
:: Symbol = POpen | PClose | CBOpen | CBClose | SBOpen | SBClose | Comma | Semicolon
		 | Identifier String | Integer Int | Op Operator
		 | KIf | KElse | KWhile | KReturn
		 | KVoid | KInt | KBool
		 | KTrue | KFalse | KAssign
		 | KType
:: Operator = Plus | Min | Mul | Div | Mod | Eq | LT | GT | LTE | GTE | NEq | And | Or | Cons | Not

:: CharMeta = { c :: Char, l :: Int, col :: Int }

derive gEq Symbol, Operator

tokenize :: ([String] -> [Token])
