definition module IRBuilder

import SemanticAnalyzer, Parser

// Change the AST into a intermediate representation to make code generation easier.

:: IR :== [IRFun]
:: IRFun = { name :: Id, blocks :: [Block]} // Id from parser is unique, since overloading is not allowed.
:: Block = { name :: Id, commands :: [Command]} // Generated Id.
:: Command = CExp [CExp] | CAssing Int | CAssingl Int  // CAssing global and CAssingl local, same as read and readl.
	| Branch Id | BranchIf Id | BranchIfElse Id Id
	| CFCall Id | CReturn | CReturne
	| Link Int // Reserver room on the stack for a number of local vars.
	| Label Id // Used to return from if and while functions.
	| Drop Int // Used to drop parameters after usage.
	| Swap // Swap topmost values on stack.
	| BranchMatch [(Int, Id)] // CExp is the value to match against, Int the count number of the algebraic type and Id the label to jump at.
:: CExp = Read Int	// Read a global var and put it on the stack.
	| Readl Int // Read a local var or param. (First param is -n, second -n+1, ... last param is -1, first local var is 1, second 2...) 0 is the return adress on the stack.
	| EOp2 Op2 | EOp1 Op1	// Call operator n. (+, -, *, / etc. All build in operators).
	| EFCall Id	// Call function Id. We add a function to generate Tupels and Lists.
	| EFCallD Id Int	// Call function Id, drops Int params from stack before calling RR.
	| Put Int	// All values are transformed to Int, this Int could be a Int, a Bool or a Char. Tupels and list's do not exist as primitives but can be created by functions and read from vars.
	| Drope Int
// Example of Command:
/*
f(Int x)
Int y = x + 5;
If (x > 8) { return True; } { return False; }

Would change into:
[{name = f, blocks = [{ name = "f_b1", commands = [CExp [Read "x", Put 5, Op '+'], Assing "y", CExp [Read "x", Put 8, Op '>'] JumpTrue "f_b2" "f_b3"]}
					, { name = "f_b2", commands = [CExp [Put 'True'], Return]}
					, { name = "f_b3", commands = [CExp [Put 'False'], Return]}]}]
*/

toIR :: Prog -> IR
