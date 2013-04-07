definition module Parser

//import Tokenizer
from Tokenizer import :: Token
import Result
import StdMaybe
import GenEq

:: Prog :== [Decl]
:: Decl = V VarDecl | F FunDecl
:: VarDecl  = { type :: Type, name :: Id, exp :: Exp }
:: FunDecl = { retType :: RetType, funName :: Id, args :: [FArg], vars ::[VarDecl], stmts :: [Stmt] }
:: RetType = RT Type | TVoid
:: Type = TInt | TBool | TTup (Type, Type) | TList Type | TId Id | TFun RetType [Type] | TEmpty | TFixed Id
:: FArg = { argType :: Type, argName :: Id }
:: Stmt = Block [Stmt] | If Exp Stmt | Ife Exp Stmt Stmt | While Exp Stmt | Ass Id Exp
			| SFC FunCall | Return | Returne Exp
:: Exp = I Id | Op2 Exp Op2 Exp | Op1 Op1 Exp | EInt Int | EFalse | ETrue | EBrace Exp | EFC FunCall
			| EBlock | Tup Exp Exp
:: FunCall = { callName :: String, callArgs :: [Exp] }
:: Op2 = PPlus | PMin | PMul | PDiv | PMod | PEq | PLT | PGT | PLTE | PGTE | PNEq | PAnd | POr | PCons
:: Op1 = PNot | PNeg
:: Id :== String

:: Priority = PAll | PBlock | PStatement | PBrace

parse :: (Result [Token]) -> Result (Prog)


(~>) infixl 7 :: (Maybe a, [String], [Token]) ([Token] -> (Maybe b, [String], [Token])) -> (Maybe a, [String], [Token])

parsePClose :: [Token] -> (Maybe Bool, [String], [Token])

derive gEq Decl
