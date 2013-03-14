definition module Parser

//import Tokenizer
from Tokenizer import :: TokenOnLine
import Result
import StdMaybe

:: Prog = P [Decl]
:: Decl = V VarDecl | F FunDecl
:: VarDecl  = VD Type Id Exp
:: FunDecl = Fun RetType Id [FArgs] [VarDecl] [Stmt]
:: RetType = RT Type | PVoid
:: Type = TInt | PBool | TTup (Type, Type) | TList Type | TId Id
:: FArgs = FA Type Id
:: Stmt = Block [Stmt] | If Exp Stmt | Ife Exp Stmt Stmt | While Exp Stmt | Ass Id Exp
			| SFC FunCall | Return | Returne Exp
:: Exp = I Id | Op2 Exp Op2 Exp | Op1 Op1 Exp | EInt | EFalse | ETrue | EBrace Exp | EFC FunCall
			| EBlock | Tup Exp Exp
:: FunCall = FC Id [ActArgs]
:: ActArgs = AA Exp
:: Op2 = PPlus | PMin | PMul | PDiv | PMod | PEq | PLT | PGT | PLTE | PGTE | PNEq | PAnd | POr
:: Op1 = PCons | PNot
:: Id = PId String

:: Priority = PAll | PBlock | PStatement | PBrace

parse :: (Result [TokenOnLine]) -> Result (Maybe Prog)