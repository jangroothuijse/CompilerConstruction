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
	toString (Sbra s) = "bra " +++ s
	toString (Sbrf s) = "brf " +++ s
	toString (Sbrt s) = "brt " +++ s
	toString (Sbsr s) = "bsr " +++ s
	toString Sdiv = "div"
	toString Seq = "eq"	
	toString Sge = "ge"
	toString Sgt = "gt"
	toString Shalt = "halt"
	toString Sjsr = "jsr"
	toString (Slda i) = "lda " +++ (toString i)
	toString Sldaa = "ldaa"
	toString (Sldc i) = "ldc " +++ (toString i)
	toString (Sldh i) = "ldh " +++ (toString i)
	toString (Sldmh i j) = "ldmh " +++ (toString i) +++ " " +++ (toString j)
	toString (Sldl i) = "ldl " +++ (toString i)
	toString Sldla = "ldla"
	toString Sldma = "ldma"	
	toString Sldml = "ldml"
	toString Sldms = "ldms"
	toString (Sldr i) = "ldr " +++ (toString i)
	toString SldrRR = "ldr RR"
	toString Sldrr = "ldrr"
	toString (Slds i) = "lds " +++ (toString i)
	toString (Sldsa i) = "ldsa " +++ (toString i)
	toString Sle = "le"
	toString (Slink i) = "link " +++ (toString i)
	toString Slt = "lt"
	toString Smod = "mod"
	toString Smul = "mul"
	toString Sne = "ne"
	toString Sneg = "neg"
	toString Snop = "nop"
	toString Snot = "not"
	toString Sor = "or"
	toString Sret = "ret"
	toString (Ssta i) = "sta " +++ (toString i)
	toString Ssth = "sth"
	toString (Sstmh i) = "stmh " +++ (toString i)
	toString (Sstl i) = "stl " +++ (toString i)
	toString Sstma = "stma"
	toString Sstml = "stml"
	toString Sstms = "stms"
	toString (Sstr i)=  "str " +++ (toString i)
	toString SstrRR = "str RR"
	toString Ssts = "sts"
	toString Ssub = "sub"
	toString Sswp = "swp"
	toString Sswpr = "swpr"
	toString Sswprr = "swprr"
	toString (Strap i) = "trap " +++ (toString i)
	toString Sunlink = "unlink"
	toString Sxor = "xor"
