definition module SSMCodeGen

import IRBuilder, Parser

toSSMCode :: IR -> SSMCode

:: SSMCode :== [SSMCommands]
:: SSMCommands = SL Id SSMIns | S SSMIns
:: SSMIns = Sadd | Sajs | Sand | Sannote | Sbra | Sbrf | Sbrt | Sbsr | Sdiv
		| Seq | Sge | Sgt | Shalt | Sjsr | Slda | Sldaa | Sldc | Sldl | Sldla
		| Sldma | Sldml | Sldms | Sldr | Sldrr | Slds | Sldsa | Sle | Slink
		| Slt | Smod | Smul | Sne | Sneg | Snop | Snot | Sor | Sret | Ssta
		| Sstl | Sstma | Sstml | Sstms | Sstr | Ssts | Ssub | Sswp | Sswpr
		| Sswprr | Strap | Sunlink | Sxor
