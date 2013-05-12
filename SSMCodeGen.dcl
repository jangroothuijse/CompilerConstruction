definition module SSMCodeGen

import IRBuilder, Parser

toSSMCode :: IR -> SSMCode

:: SSMCode :== [SSMCommands]
:: SSMCommands = SL Id SSMIns | S SSMIns
:: SSMIns = Sadd | Sajs Int | Sand | Sannote | Sbra | Sbrf Id | Sbrt Id | Sbsr Id | Sdiv
		| Seq | Sge | Sgt | Shalt | Sjsr | Slda | Sldaa | Sldc Int | Sldl Int | Sldla
		| Sldma | Sldml | Sldms | Sldr Int | SldrRR | Sldrr | Slds | Sldsa | Sle | Slink Int
		| Slt | Smod | Smul | Sne | Sneg | Snop | Snot | Sor | Sret | Ssta Int
		| Sstl | Sstma | Sstml | Sstms | Sstr Int | SstrRR | Ssts | Ssub | Sswp | Sswpr
		| Sswprr | Strap | Sunlink | Sxor
