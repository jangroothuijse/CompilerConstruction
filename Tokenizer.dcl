definition module Tokenizer

import StdMaybe
import Result
import GenEq

:: Token = { token :: Symbol, line :: Int, column :: Int }
:: Symbol = POpen | PClose | CBOpen | CBClose | SBOpen | SBClose | Comma | Semicolon
		 | Identifier String | Integer Int | Op Operator
		 | KIf | KElse | KWhile | KReturn
		 | KVoid | KInt | KBool
		 | KTrue | KFalse | KAssign
:: Operator = Plus | Min | Mul | Div | Mod | Eq | LT | GT | LTE | GTE | NEq | And | Or | Cons | Not

derive gEq Symbol, Operator

tokenizer :: (Result [String]) -> Result [Token]
