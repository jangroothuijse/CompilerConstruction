definition module IRBuilder

import SemanticAnalyzer, Parser
from Parser import :: Prog, :: Decl

// Change the AST into a intermediate representation to make code generation easier.

/*
Todo:
Reorder operators for code.
Label blocks.
Reference command flow operators to block labels (if/while/funcall).
Mark local and global variable references.
*/

:: IR :== [IRFun]
:: IRFun = { name :: CId, blocks :: [Block]} // Id from parser is unique, since overloading is not allowed
:: Block = { name :: CId, commands :: [Command], depth :: Int} // Generated Id, depth = calldepth of block, required for access to local variables, parameters and for returning to previouse function.
// TODO: New plan: Replace Jump with the target Command for now, so JumpTrue [Command] [Command] for now. Possible add a Jump with 3 commands, one for true, one for false, and one for afterwards.
// Other problem to solve: Void function that doenst end in a return;
:: Command = CExp [CExp] | CAssing CId | CAssingl Int  // CAssing global and CAssingl local, same as read and readl.
	| Branch CId | BranchIf CId | BranchIfElse CId CId | BranchWhile [CExp] CId // JumpWhile contains a CExp because it need to include the CExp in the loop.
	| CFCall CId | CReturn | CDrop Int // Drop unused result from stack.
:: CExp = Read CId	// Read a var and put it on the stack.
	| Readl Int // Read a local var or param. (First param is -n, second (-n)+1, ... last param is -1, first local var is 0, second 1, ...)
	| EOp2 Op2 | EOp1 Op1	// Call operator n. (+, -, *, / etc. All build in operators).
	| EFCall CId	// Call function Id. We add a function to generate Tupels and Lists.
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
:: CId :== String

toIR :: (Prog, *UEnv) -> (IR, *UEnv)
