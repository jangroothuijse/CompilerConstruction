definition module Parser

//import Tokenizer
from Tokenizer import :: Token
import Result
import StdMaybe

:: Prog :== [Decl]
:: Decl = V VarDecl | F FunDecl
:: VarDecl  = VD Type Id Exp
:: FunDecl = Fun RetType Id [FArgs] [VarDecl] [Stmt]
:: RetType = RT Type | PVoid
:: Type = TInt | PBool | TTup (Type, Type) | TList Type | TId Id
:: FArgs = FA Type Id
:: Stmt = Block [Stmt] | If Exp Stmt | Ife Exp Stmt Stmt | While Exp Stmt | Ass Id Exp
			| SFC FunCall | Return | Returne Exp
:: Exp = I Id | Op2 Exp Op2 Exp | Op1 Op1 Exp | EInt Int | EFalse | ETrue | EBrace Exp | EFC FunCall
			| EBlock | Tup Exp Exp
:: FunCall = FC Id [ActArgs]
:: ActArgs = AA Exp
:: Op2 = PPlus | PMin | PMul | PDiv | PMod | PEq | PLT | PGT | PLTE | PGTE | PNEq | PAnd | POr | PCons
:: Op1 = PNot | PNeg
:: Id :== String

:: Priority = PAll | PBlock | PStatement | PBrace

parse :: (Result [Token]) -> Result (Maybe Prog)


(~>) infixl 7 :: (Maybe a, [String], [Token]) ([Token] -> (Maybe b, [String], [Token])) -> (Maybe a, [String], [Token])

parsePClose :: [Token] -> (Maybe Bool, [String], [Token])
