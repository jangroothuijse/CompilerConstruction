definition module IRBuilder

import SemanticAnalyzer, Parser

// Change the AST into a intermediate representation to make code generation easier.

/*
Todo:
Reorder operators for code.
Label blocks.
Reference command flow operators to block labels (if/while/funcall).
Mark local and global variable references.
*/

:: IR :== [IRFun]
:: IRFun = { name :: Id, blocks :: [Block]} // Id from parser is unique, since overloading is not allowed.
:: Block = { name :: Id, commands :: [Command]} // Generated Id.
// Other problem to solve: Void function that doenst end in a return;
:: Command = CExp [CExp] | CAssing Int | CAssingl Int  // CAssing global and CAssingl local, same as read and readl.
	| Branch Id | BranchIf Id | BranchIfElse Id Id
	| CFCall Id | CReturn | CReturne
	| Link Int // (Un)reserver room on the stack for a number of local vars.
	| Label Id // Used to return from if and whil functions.
	| Drop Int // Used to drop parameters after usage.
	| Swap // Swap topmost values on stack.
:: CExp = Read Int	// Read a global var and put it on the stack.
	| Readl Int // Read a local var or param. (First param is -n, second -n+1, ... last param is -1, first local var is 1, second 2...) 0 is the return adress on the stack.
	| EOp2 Op2 | EOp1 Op1	// Call operator n. (+, -, *, / etc. All build in operators).
	| EFCall Id	// Call function Id. We add a function to generate Tupels and Lists.
	| Put Int	// All values are transformed to Int, this Int could be a Int, a Bool or a Char. Tupels and list's do not exist as primitives but can be created by functions and read from vars.
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

/* Notes:
Need Label (Id) for while and if statements, Could be generated for already unique name if multiple are needed.
Blocks called from if & while statements need to return to the orginal.
*/

toIR :: Prog -> IR
