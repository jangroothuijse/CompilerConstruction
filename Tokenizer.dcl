definition module Tokenizer

import StdEnv
import StdMaybe

:: TokenOnLine = { token :: Token, line :: Int }
:: Token = Popen | PClose | CBOpen | CBClose | SBOpen | SBClose | Comma | Semicolon
		 | Identifier String | Integer Int | Op Operator
		 | KIf | KElse | KWhile | KReturn
		 | KVoid | KInt | KBool
		 | KTrue | KFalse | KAssign
:: Operator = Plus | Min | Mul | Div | Mod | Eq | LT | GT | LTE | GTE | NEq | And | Or | Cons | Not

tokenizer :: ([String] -> [TokenOnLine])
