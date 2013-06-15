implementation module SSMCodeGen

import StdEnv, IRBuilder

Start = 0

toSSMCode :: IR -> SSMCode
toSSMCode ir = OptimizeCode (progStart ++ flatten (map toSSMCodeFun ir) ++ defaultFunctions)

progStart = [S (Sldsa 1), S (Sstr 5) // store start of stack in register 5.
			, S (Sldc 0), S (Ssth), // store [] element on heap.
			 S (Sbsr "main"), S SldrRR, S (Strap 0), S Shalt] // execute main and halt.

toSSMCodeFun { blocks = blocks} = flatten (map toSSMCodeBlock blocks)

toSSMCodeBlock { name = name, commands = command} = [(SL name Snop):flatten (map (toSSMCommands) command)]

toSSMCommands :: Command -> [SSMCommands]
toSSMCommands (CExp exp) = flatten (map toSSMCommandsExp exp)
toSSMCommands (CAssing i) = [S (Sldr 5), S(Ssta (i+1))]
toSSMCommands (CAssingl i) = [S (Sstl i)]
toSSMCommands (Branch name) = [S (Sbra name)]
toSSMCommands (BranchIf name) = [S (Sbrt name)]
toSSMCommands (BranchIfElse namet namef) = [S (Slds 0), S (Sbrt namet), S (Sbrf namef)]
toSSMCommands (CFCall id) = [S (Sbsr id)]
toSSMCommands CReturn = [S Sunlink, S Sret]
toSSMCommands CReturne = [S SstrRR, S Sunlink, S Sret]
toSSMCommands (Link i) = [S (Slink i)]
toSSMCommands (Swap) = [S Sswp]
toSSMCommands (Label s) = [SL s Snop]
toSSMCommands (Drop i) = [S (Sajs (~i))]
toSSMCommands (BranchMatch [(i, name):cases]) = [S (Slds 0): S (Sldc i): S Seq: S (Sbrt name): toSSMCommands (BranchMatch cases)]
toSSMCommands (BranchMatch []) = [S (Sajs -1)]


toSSMCommandsExp :: CExp -> [SSMCommands] 
toSSMCommandsExp (Read i) = [S (Sldr 5), S (Slda (i+1))]
toSSMCommandsExp (Readl i) = [S (Sldl i)]
toSSMCommandsExp (EOp2 op2) = toSSMCommandsOp2 op2
toSSMCommandsExp (EOp1 op1) = toSSMCommandsOp1 op1
toSSMCommandsExp (EFCall name) = [S (Sbsr name), S SldrRR]
toSSMCommandsExp (Put i) = [S (Sldc i)]
toSSMCommandsExp (Drope i) = [S (Sajs (~i))]

toSSMCommandsOp2 :: Op2 -> [SSMCommands]
toSSMCommandsOp2 PPlus = [S Sadd]
toSSMCommandsOp2 PMin = [S Ssub]
toSSMCommandsOp2 PMul = [S Smul]
toSSMCommandsOp2 PDiv = [S Sdiv]
toSSMCommandsOp2 PMod = [S Smod]
toSSMCommandsOp2 PEq = [S Seq]
toSSMCommandsOp2 PLT = [S Slt]
toSSMCommandsOp2 PGT = [S Sgt]
toSSMCommandsOp2 PLTE = [S Sle]
toSSMCommandsOp2 PNEq = [S Sne]
toSSMCommandsOp2 PAnd = [S Sand]
toSSMCommandsOp2 POr = [S Sor]
toSSMCommandsOp2 PCons = [S (Sbsr "__Cons"), S (Sajs -2), S SldrRR]

toSSMCommandsOp1 :: Op1 -> [SSMCommands]
toSSMCommandsOp1 PNot = [S Snot]
toSSMCommandsOp1 PNeg = [S Sneg]

defaultFunctions = flatten [print, createEBlock, cons, createTup, createAlg, fst`, snd`, hd`, tl`, isEmpty`]

print			= [SL "print" (Slds -1), S (Strap 0), S Sret]
createEBlock	= [SL "__createEBlock" (Sldr 5), S (Slda 0), S SstrRR, S Sret]
cons			= [SL "__Cons" (Slds -2), S (Slds -2), S (Sstmh 2), S SstrRR, S Sret]
createTup		= [SL "__createTup" (Slds -2), S (Slds -2), S (Sstmh 2), S SstrRR, S Sret]
createAlg		= [SL "__createAlg" (Slds -2), S (Slds -2), S (Sstmh 2), S SstrRR, S Sret]
fst`			= [SL "fst" (Slds -1), S (Sldmh 0 1), S SstrRR, S Sret]
hd`				= [SL "hd"  (Slds -1), S (Sldmh 0 2), S (Sajs -1), S SstrRR, S Sret]
snd`			= [SL "snd" (Slds -1), S (Sldmh 0 2), S SstrRR, S (Sajs -1), S Sret]
tl`				= [SL "tl"  (Slds -1), S (Sldmh 0 2), S SstrRR, S (Sajs -1), S Sret]
isEmpty`		= [SL "isEmpty" (Slds -1), S (Sldr 5), S (Slda 0), S Seq, S SstrRR, S Sret]

OptimizeCode :: SSMCode -> SSMCode
OptimizeCode [SL i (Sbra s):S ins:xs] = OptimizeCode [SL i (Sbra s):xs]
OptimizeCode [S (Sbra s):S ins:xs] = OptimizeCode [S (Sbra s):xs]
OptimizeCode [SL i Shalt:S ins:xs] = OptimizeCode [SL i Shalt:xs]
OptimizeCode [S Shalt:S ins:xs] = OptimizeCode [S Shalt:xs]
OptimizeCode [SL i Snop:S ins:xs] = OptimizeCode [SL i ins:xs]
OptimizeCode [S Snop:xs] = OptimizeCode xs
OptimizeCode [SL i Sret:S ins:xs] = OptimizeCode [SL i Sret:xs]
OptimizeCode [S Sret:S ins:xs] = OptimizeCode [S Sret:xs]
OptimizeCode [x:xs] = [x:OptimizeCode xs]
OptimizeCode [] = []





