implementation module SSMCodeGen

import StdEnv, IRBuilder

Start = 0

toSSMCode :: IR -> SSMCode
toSSMCode ir = [S (Sldc 0), S (Ssth), S (Sbsr "main"), S SldrRR, S (Strap 0), S Shalt:flatten (map toSSMCodeFun ir)] ++ defaultFuncions

toSSMCodeFun { blocks = blocks} = flatten (map toSSMCodeBlock blocks)

toSSMCodeBlock { name = name, commands = command} = [(SL name Snop):flatten (map (toSSMCommands) command)]

toSSMCommands :: Command -> [SSMCommands]
toSSMCommands (CExp exp) = flatten (map toSSMCommandsExp exp)
toSSMCommands (CAssing i) = [S (Ssta i)]
toSSMCommands (CAssingl i) = [S (Sstl i)]
toSSMCommands (Branch name) = [S (Sbsr name)]
toSSMCommands (BranchIf name) = [S (Sbrt name), S (Sajs -1)]
toSSMCommands (BranchIfElse namet namef) = [S (Sbrt namet), S (Sbrf namef), S (Sajs -1)]
toSSMCommands (CFCall id) = [S (Sbsr id)]
toSSMCommands CReturn = [S Sret]
toSSMCommands CReturne = [S SstrRR, S Sret]
toSSMCommands (Link i) = [S (Slink i)]
toSSMCommands Unlink = [S Sunlink]
toSSMCommands (Label s) = [SL s Snop]
toSSMCommands (Drop i) = [S (Sajs i)]


toSSMCommandsExp :: CExp -> [SSMCommands] 
toSSMCommandsExp (Read i) = [S (Slda i)]
toSSMCommandsExp (Readl i) = [S (Sldl i)]
toSSMCommandsExp (EOp2 op2) = toSSMCommandsOp2 op2
toSSMCommandsExp (EOp1 op1) = toSSMCommandsOp1 op1
toSSMCommandsExp (EFCall name) = [S (Sbsr name), S SldrRR]
toSSMCommandsExp (Put i) = [S (Sldc i)]

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

defaultFuncions = flatten [print, createEBlock, cons, createTup, fst`, snd`, hd`, tl`, isEmpty`]

print			= [SL "print" (Slds -1), S (Strap 0), S Sret]
createEBlock	= [SL "__createEBlock" (Sldc 0), S (Slda 0), S (Sldh 0), S SstrRR, S Sret]
cons			= [SL "__Cons" (Slds -2), S (Slds -2), S (Sstmh 2), S SstrRR, S Sret]
createTup		= [SL "__createTup" (Slds -2), S (Slds -2), S (Sstmh 2), S SstrRR, S Sret]
fst`			= [SL "fst" (Slds -1), S (Sldmh 0 1), S SstrRR, S Sret]
hd`				= [SL "hd"  (Slds -1), S (Sldmh 0 1), S SstrRR, S Sret]
snd`			= [SL "snd" (Slds -1), S (Sldmh 0 2), S SstrRR, S (Sajs -1), S Sret]
tl`				= [SL "tl"  (Slds -1), S (Sldmh 0 2), S SstrRR, S (Sajs -1), S Sret]
isEmpty`		= [SL "isEmpty" (Slds -1), S (Sldc 0), S (Slda 0), S Seq, S SstrRR, S Sret]

