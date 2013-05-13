definition module SSMCodeGen

import IRBuilder, Parser

toSSMCode :: IR -> SSMCode

:: SSMCode :== [SSMCommands]
:: SSMCommands = SL Id SSMIns | S SSMIns
:: SSMIns = Sadd | Sajs Int | Sand | Sannote | Sbra String | Sbrf Id | Sbrt Id | Sbsr Id | Sdiv
		| Seq | Sge | Sgt | Shalt | Sjsr | Slda Int | Sldaa | Sldc Int | Sldh Int | Sldmh Int Int
		| Sldl Int | Sldla | Sldma | Sldml | Sldms | Sldr Int | SldrRR | Sldrr | Slds Int | Sldsa Int
		| Sle | Slink Int | Ssth | Sstmh Int | Slt | Smod | Smul | Sne | Sneg | Snop | Snot | Sor
		| Sret | Ssta Int | Sstl Int | Sstma | Sstml | Sstms | Sstr Int | SstrRR | Ssts | Ssub
		| Sswp | Sswpr | Sswprr | Strap Int | Sunlink | Sxor
