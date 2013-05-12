implementation module SSMWriter

import StdEnv, SSMCodeGen

instance writeSSM SSMCode where writeSSM f l = foldl (writeSSM) f l
instance writeSSM SSMCommands where
	writeSSM f (SL l ins) = f <<< (l +++ ":\t" +++ (toString ins) +++ ";\n")
	writeSSM f (S ins) = f <<< ("\t\t" +++ (toString ins) +++ ";\n")
	
instance toString SSMIns where
	toString Sadd = "add"
	toString (Sajs i) = "ajs " +++ (toString i)
	toString Sand = "and"
	toString Sannote = "annote"
	toString Sbra = "bra"
	toString (Sbrf i) = "brf " +++ i
	toString (Sbrt i) = "brt " +++ i
	toString (Sbsr i) = "bsr " +++ i
	toString Sdiv = "div"
	toString Seq = "eq"	
	toString Sge = "ge"
	toString Shalt = "halt"
	toString Sjsr = "jsr"
	toString Slda = "lda"
	toString Sldaa = "ldaa"
	toString (Sldc i) = "ldc " +++ (toString i)
	toString (Sldl i) = "ldl " +++ (toString i)
	toString Sldla = "ldla"
	toString Sldma = "ldma"
	toString Sldml = "ldml"
	toString Sldms = "ldms"
	toString (Sldr i) = "ldr " +++ (toString i)
	toString SldrRR = "ldrRR"
	toString Sldrr = "ldrr"
	toString Slds = "lds"
	toString Sldsa = "ldsa"
	toString Sle = "le"
	toString Slink = "link"
	toString Slt = "lt"
	toString Smod = "mod"
	toString Smul = "mul"
	toString Sne = "ne"
	toString Sneg = "neg"
	toString Snop = "nop"
	toString Snot = "not"
	toString Sor = "or"
	toString Sret = "ret"
	toString Ssta = "sta"
	toString Sstl = "stl"
	toString Sstma = "stma"
	toString Sstml = "stml"
	toString Sstms = "stms"
	toString Sstr = "str"
	toString Ssts = "sts"
	toString Ssub = "sub"
	toString Sswp = "swp"
	toString Sswpr = "swpr"
	toString Sswprr = "swprr"
	toString Strap = "trap"
	toString Sunlink = "unlink"
	toString Sxor = "xor"
