definition module IRBuilder

// Change the AST into a intermediate representation to make code generation easier.

/*
Todo:
Reorder operators for code.
Label blocks.
Reference command flow operators to block labels (if/while/funcall).
Mark local and global variable references.
*/

:: IR :== [IRFun]
:: IRFun = { name :: Id, blocks :: [Block]} // Id from parser is unique, since overloading is not allowed
:: Block = { name :: Id, commands :: [Command]} // Generated Id
:: Command = CExp [CExp] | Assing Id | JumpTrue Id Id // JumpTrue can be used for If, If..Else and while. First ID is for when prev. command was True and second Id for when it was false.
	| CFCall Id | Return Int // The Int in Return represent the number of parameters (to remove them from stack).
:: CExp =
	  Read Id	// Read a var and put it on the stack.
	| Readl Int // Read a local var or param. (First param is -n, second (-n)+1, ... last param is -1, first local var is 0, second 1, ...)
	| EOp Int	// Call operator n. (+, -, *, / etc. All build in operators).
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
:: Id :== String