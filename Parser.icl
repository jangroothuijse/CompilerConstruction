implementation module Parser

import StdEnv, StdMaybe, GenEq, Tokenizer

:: Unit = Unit
:: PR a = { tokens :: [Token], result :: a }
getResult r = r.PR.result
getTokens r = r.PR.tokens
pr r t = { PR | result = r, tokens = t }
:: Parse a :== ([Token] -> PR a)

parse :: [Token] -> Prog
parse [] = []
parse tokens = let r = parseDecl tokens in [r.PR.result : parse r.PR.tokens]
	
parseDecl :: [Token] -> PR Decl
parseDecl [] = parseError [] "expecting declaration"
parseDecl t
|		(hd t).Token.token === KType = parseAlg (tl t)
|		(hd t).Token.token === KVoid = let iden = parseId (tl t) in parseFunDecl TVoid iden.result iden.tokens
#		type = parseType t		
#		iden = parseId type.PR.tokens
|		isEmpty iden.tokens =  abort "parseDecl Failed"
|		(hd iden.tokens).Token.token === KAssign = let exp = parseExp (tl iden.PR.tokens) in let semi = parseSymbol Semicolon exp.PR.tokens in
			 { PR | tokens = semi.PR.tokens, result = V { VarDecl | type = type.result, name = iden.PR.result, exp = exp.PR.result } }
=		parseFunDecl (RT type.result) iden.result iden.tokens

parseSymbol :: Symbol [Token] -> PR Unit
parseSymbol t [{token = s}:xs] = if (t === s) { PR | result = Unit, tokens = xs } (parseError xs ("Unexpected token"))
parseSymbol t _ = parseError [] ""

parseFunDecl :: RetType Id [Token] -> PR Decl
parseFunDecl type name [] = parseError [] "expecting function body"
parseFunDecl type name tokens = { PR | result = F { retType = type, funName = name, args = fargs.PR.result, vars = vars.PR.result, stmts = stmts.PR.result, fline = (hd tokens).line, fcolumn = (hd tokens).column }, tokens = stmts.PR.tokens } where
	fargs = parseFArgs tokens
	funBegin = parseSymbol CBOpen fargs.PR.tokens
	vars = parseVars funBegin.PR.tokens					// <- must be lazy
	stmts = parseStmts vars.PR.tokens					// <- must be lazy
	
parseFArgs :: [Token] -> PR [FArg]
parseFArgs tokens = f argsBegin.tokens [] where
	argsBegin = parseSymbol POpen tokens
	f :: [Token] ![FArg] -> PR [FArg]
	f [{token = PClose}:xs] acc = { PR | result = acc, tokens = xs }
	f [x=:{token = Comma}:xs] [] = parseError [x:xs] "Argument list cannot start with comma"
	f [x=:{token = Comma}:xs] acc = f xs acc
	f tokens acc = let t = parseType tokens in let i = parseId t.PR.tokens in f i.PR.tokens (acc ++ [{ FArg | argType = t.PR.result, argName = i.PR.result }]) 
	
parseVars :: [Token] -> PR [VarDecl]	// Needs to lookout for the change from vardecls to stmts, which is hard to detect.
parseVars tokens
# type = parseMaybeType tokens
| isNothing type = { PR | result = [], tokens = tokens }
# t = fromJust type
= case t.PR.tokens of 
	[{token = (Identifier i)}:[{token = KAssign}:xs]] = 
		{ PR | result = [{VarDecl | type = t.PR.result, name = i, exp = exp.PR.result }:next.PR.result], tokens = next.PR.tokens } where 
		exp = parseExp xs
		semi = parseSymbol Semicolon exp.PR.tokens
		next = parseVars semi.PR.tokens
	_	= { PR | result = [], tokens = tokens }

parseAlg :: [Token] -> PR Decl
parseAlg [{token = Identifier name}:xs]
# { PR | result = poly, tokens = t1 } = parsePoly [] xs
# { PR | result = parts, tokens = t2 } = parsePart [] (parseSymbol KAssign t1).tokens
= pr (A { AlgDecl | adname = name, poly = poly, parts = parts }) t2
where
	parsePoly :: [Id] [Token] -> PR [Id]
	parsePoly acc [{token = Identifier n}:xs] = parsePoly [n:acc] xs 
	parsePoly acc [] = parseError [] "expected Algebraic type declaration, found EOF"
	parsePoly acc tokens = pr acc tokens
	parsePart :: [AlgPart] [Token] -> PR [AlgPart]
	parsePart acc [{token = Semicolon }:xs] = pr acc xs
	parsePart acc [{token = Bar }:xs] = parsePart acc xs
	parsePart acc [{token = Identifier n }:xs] = let mt = parseMaybeType xs in case mt of
		(Just t) = parsePart (acc ++ [{ AlgPart | apname = n, atype = t.PR.result }]) t.PR.tokens
		Nothing = parsePart (acc ++ [{ AlgPart | apname = n, atype = TEmpty }]) xs
	
parseType :: [Token] -> PR Type
parseType [{token = KBool}:xs] = { PR | result = TBool, tokens = xs }
parseType [{token = KInt}:xs] = { PR | result = TInt, tokens = xs }
parseType [{token = (Identifier i)}:xs] = { PR | result = (TId i), tokens = xs }
parseType [{token = SBOpen}:xs] = let i = parseType xs in { PR | result = (TList i.PR.result), tokens = (parseSymbol SBClose i.PR.tokens).PR.tokens }
parseType [{token = POpen}:xs]
# { result = type1, tokens = xs } = parseType xs
| (hd xs).token === Comma 
	# { result = type2, tokens = xs } = parseType (tl xs)
	= { PR | result = TTup (type1, type2), tokens = xs }
| (hd xs).token === PClose = case type1 of
	(TId i) = { PR | result = TAlg i [], tokens = tl xs }
= case type1 of
	(TId i) = let { PR | result = result, tokens = tokens } = f [] xs in { PR | result = TAlg i result, tokens = tokens } where
		f :: [Type] [Token] -> PR [Type]
		f acc [{ token = PClose }:tokens] = { PR | tokens = tokens, result = acc }
		f acc t = let { PR | tokens = tokens, result = result } = parseType t in f [result:acc] tokens
	_ = parseError xs " Failed to parse type"
parseType t = parseError t " Failed to parse type"

propNothing :: (Maybe a) b -> Maybe b
propNothing x y = if (isNothing x) Nothing (Just y)

parseMaybeType :: [Token] -> Maybe (PR Type)	// Checks for a type without aborting
parseMaybeType [{token = KBool}:xs] = Just { PR | result = TBool, tokens = xs }
parseMaybeType [{token = KInt}:xs] = Just { PR | result = TInt, tokens = xs }
parseMaybeType [{token = (Identifier i)}:xs] = Just { PR | result = (TId i), tokens = xs }
parseMaybeType [{token = SBOpen}:xs] = let i = parseMaybeType xs in propNothing i { PR | result = TList (fromJust i).PR.result, tokens = (parseSymbol SBClose (fromJust i).PR.tokens).PR.tokens }
parseMaybeType [{token = POpen}:xs] 
# t1 = parseMaybeType xs
| isNothing t1 = Nothing
| (hd (fromJust t1).PR.tokens).token === Comma
	# t2 = parseMaybeType (tl (fromJust t1).PR.tokens)
	| isNothing t2 = Nothing
	= Just { PR | result = TTup ((fromJust t1).PR.result, (fromJust t2).PR.result), tokens = tl (fromJust t2).PR.tokens }
= case t1 of 
	(Just { tokens = tokens, result = TId adname }) = 
		let r = f [] tokens in 
			if (isJust r) 
				(Just (pr (TAlg adname (fromJust r).PR.result) (fromJust r).PR.tokens)) 
				Nothing
	where
		f :: [Type] [Token] -> Maybe (PR [Type])
		f acc [{token = PClose}:tokens] = Just { PR | tokens = tokens, result = acc }
		f acc [] = Nothing
		f acc tokens
		# t3 = parseMaybeType tokens
		| isNothing t3 = Nothing
		= f (acc ++ [(fromJust t3).PR.result]) (fromJust t3).PR.tokens
	_ = Nothing
parseMaybeType t = Nothing

parseId [{token = Identifier i}:xs] = { PR | result = i, tokens = xs }
parseId tokens = parseError tokens "expected identifier"

parseStmtsRec resultHead tokens = let next = parseStmts tokens in { PR | result = [resultHead:next.PR.result], tokens = next.PR.tokens }

parseStmts :: [Token] -> PR [Stmt]
parseStmts [{token = CBClose}:xs] = { PR | result = [], tokens = xs }
parseStmts tokens = let s = parseStmt tokens in parseStmtsRec s.PR.result s.PR.tokens

parseStmt :: [Token] -> PR Stmt
parseStmt [{token = CBOpen}:xs] = let block = parseStmts xs in { PR | result = Block block.PR.result, tokens = block.PR.tokens }
parseStmt [{token = KReturn}:[{token = Semicolon}:xs]] = { PR | result = Return, tokens = xs }
parseStmt [{token = KReturn}:xs] = let e = (parseExp ~> (parseSymbol Semicolon)) xs in { PR | result = Returne e.PR.result, tokens = e.PR.tokens }
parseStmt [{token = KWhile}:xs] = { PR | result = (While e.PR.result stmts.PR.result), tokens = stmts.PR.tokens } where 
	e = ((parseSymbol POpen) ~>- (parseExp ~> (parseSymbol PClose))) xs
	stmts = parseStmt e.PR.tokens
parseStmt [{token = KIf}:xs] = if ((hd thenBlock.PR.tokens).Token.token === KElse)
		{ PR | result = Ife e.PR.result thenBlock.PR.result elseBlock.PR.result, tokens = elseBlock.PR.tokens }
		{ PR | result = If e.PR.result thenBlock.PR.result, tokens = thenBlock.PR.tokens } where
		e = ((parseSymbol POpen) ~>- (parseExp ~> (parseSymbol PClose))) xs
		thenBlock = parseStmt e.PR.tokens
		elseBlock = parseStmt (tl thenBlock.PR.tokens)
parseStmt [{token = KMatch}:xs]
# { PR | tokens = xs, result = i } = ((parseSymbol POpen) ~>- (parseId ~> ((parseSymbol PClose) ~> (parseSymbol CBOpen)))) xs
= let { PR | result = cases, tokens = tokens } = f [] xs in { PR | result = Match i cases, tokens = tokens } where
	f :: [Case] [Token] -> PR [Case]
	f acc [{token = CBClose}:xs] = pr acc xs
	f acc [{token = KCase}:[{token = POpen}:xs]] = case xs of
		[{token = Identifier cn }:[{token = PClose }:xxs]]
			# { PR | result = stmts, tokens = xxs } = parseStmt xxs
			= f (acc ++ [Case cn [] stmts]) xxs
		[{token = Identifier cn }:[{token = Identifier varName }:xxs]]
			# { PR | result = stmts, tokens = xxs } = ((parseSymbol PClose) ~>- parseStmt) xxs
			= f (acc ++ [Case cn [varName] stmts]) xxs
parseStmt [{token = (Identifier i)}:[{token = KAssign}:xs]] = let e = (parseExp ~> (parseSymbol Semicolon)) xs in { PR | result = Ass i e.PR.result, tokens = e.PR.tokens }
parseStmt [{token = (Identifier i)}:[{token = POpen}:xs]] = { PR | result = SFC { FunCall | callName = i, callArgs = fa }, tokens = (parseSymbol Semicolon xxs).tokens }
where { result = fa, tokens = xxs } = parseFArgExps xs
parseStmt tokens = parseError tokens "statement expected while parsing statements"

parseFArgExps :: [Token] -> PR [Exp]
parseFArgExps xs = f xs [] where
	f :: [Token] ![Exp] -> PR [Exp]
	f [{token = PClose}:xs] acc = pr acc xs
	f xs=:[{token = Comma}:_] [] = parseError xs " unexpected comma "
	f xs [] = let e = parseExp xs in f e.PR.tokens [e.PR.result]
	f [{token = Comma}:xs] acc = let e = parseExp xs in f e.PR.tokens (acc ++ [e.PR.result])

(~>) infixl 7 :: (Parse a) (Parse b) -> ([Token] -> PR a)			// Requires ab, returns a
(~>) a b = (\t -> let ra = a t in let rb = b ra.PR.tokens in { PR | ra & tokens = rb.PR.tokens })

(~>#) infixl 7 :: (Parse a) (Parse b) -> ([Token] -> PR (a, b))		// Requires ab, returns (a, b)
(~>#) a b = (\t -> let ra = a t in let rb = b ra.PR.tokens in { PR | result = (ra.PR.result, rb.PR.result), tokens = rb.PR.tokens } )

(~>-) infixl 7 :: (Parse a) (Parse b) -> ([Token] -> PR b)		// Requires ab, returns b
(~>-) a b = (\t -> let ra = a t in b ra.PR.tokens)

parseError :: [Token] String -> a
parseError [] e = abort ("PARSE ERROR: Unexpected end of file " +++ e +++ "\n")
parseError [x:xs] e = abort ("PARSE ERROR: " +++ e +++ " on line " +++ (toString x.Token.line) +++ " column " +++ (toString x.Token.column) +++ "\n")

parseExp :: [Token] -> PR Exp
parseExp t = let e = parseAndExp t in if (isEmpty e.PR.tokens || not ((hd e.PR.tokens).Token.token === (Op Cons))) (pr e.PR.result e.PR.tokens)
	(let ex = parseExp (tl e.PR.tokens) in { PR | result = e2 (Op2 e.PR.result PCons ex.PR.result) t, tokens = ex.PR.tokens }) // <- Cons right asso
where
	e2 e1 [] = abort "No e2"
	e2 e1 t = { Exp | ex = e1, eline = (hd t).Token.line, ecolumn = (hd t).Token.column }	
	f op2 tx e g = let ex = g tx in { PR | result = e2 (Op2 e.PR.result op2 ex.PR.result) t, tokens = ex.PR.tokens }
	parseAndExp t = let e = parseOrExp t in parseAndExpMore e.PR.result e.PR.tokens
	parseAndExpMore acc t=:[{token = Op And}:tx] = let e = parseOrExp tx in parseAndExpMore (e2 (Op2 acc PAnd e.PR.result) tx) e.tokens
	parseAndExpMore acc t = (pr acc t)
	parseOrExp t = let e = parseRelExp t in parseOrExpMore e.PR.result e.PR.tokens
	parseOrExpMore acc t=:[{token = Op Or}:tx] = let e = parseRelExp tx in parseOrExpMore (e2 (Op2 acc POr e.PR.result) tx) e.tokens
	parseOrExpMore acc t = (pr acc t)
	parseRelExp t = let e = parseSumExp t in parseRelExpMore e.PR.result e.PR.tokens
	parseRelExpMore acc t=:[{token = Op op}:tx] = case op of
			Eq	= let e = parseTermExp tx in parseRelExpMore (e2 (Op2 acc PEq e.PR.result) tx) e.tokens
			LT	= let e = parseTermExp tx in parseRelExpMore (e2 (Op2 acc PLT e.PR.result) tx) e.tokens
			GT	= let e = parseTermExp tx in parseRelExpMore (e2 (Op2 acc PGT e.PR.result) tx) e.tokens
			LTE = let e = parseTermExp tx in parseRelExpMore (e2 (Op2 acc PLTE e.PR.result) tx) e.tokens
			GTE = let e = parseTermExp tx in parseRelExpMore (e2 (Op2 acc PGTE e.PR.result) tx) e.tokens
			NEq = let e = parseTermExp tx in parseRelExpMore (e2 (Op2 acc PNEq e.PR.result) tx) e.tokens
			_	= (pr acc t)
	parseRelExpMore acc t 	=	(pr acc t)
	parseSumExp t = let e = parseTermExp t in parseSumExpMore e.PR.result e.PR.tokens	
	parseSumExpMore acc t=:[{token = Op op}:tx] = case op of
			Plus	= let e = parseTermExp tx in parseSumExpMore (e2 (Op2 acc PPlus e.PR.result) tx) e.tokens
			Min		= let e = parseTermExp tx in parseSumExpMore (e2 (Op2 acc PMin e.PR.result) tx) e.tokens
			_	= (pr acc t)
	parseSumExpMore acc t 	=	(pr acc t)
	parseTermExp t = let e = parseFactorExp t in parseTermExpMore e.PR.result e.PR.tokens	
	parseTermExpMore acc t=:[{token = Op op}:tx] = case op of
			Mul		= let e = parseFactorExp tx in parseTermExpMore (e2 (Op2 acc PMul e.PR.result) tx) e.tokens
			Div		= let e = parseFactorExp tx in parseTermExpMore (e2 (Op2 acc PDiv e.PR.result) tx) e.tokens
			Mod		= let e = parseFactorExp tx in parseTermExpMore (e2 (Op2 acc PDiv e.PR.result) tx) e.tokens
			_	= (pr acc t)
	parseTermExpMore acc t 	= (pr acc t)	
	parseFactorExp tokens=:[{token = Op Not}:xs] = let e = parseExp xs in pr (e2 (Op1 PNot e.PR.result) tokens) e.PR.tokens
	parseFactorExp tokens=:[{token = Op Min}:xs] = let e = parseExp xs in pr (e2 (Op1 PNeg e.PR.result) tokens) e.PR.tokens
	parseFactorExp tokens=:[{token = Integer z}:xs] = pr (e2 (EInt z) tokens) xs
	parseFactorExp tokens=:[{token = KTrue}:xs] = pr (e2 ETrue tokens) xs
	parseFactorExp tokens=:[{token = KFalse}:xs] = pr (e2 EFalse tokens) xs
	parseFactorExp tokens=:[{token = SBOpen}:[{token = SBClose}:xs]] = pr (e2 EBlock tokens) xs
	parseFactorExp tokens=:[{token = KNew}:[{token = Identifier c}:xs]] = case xs of
		[{token = KNew}:_] = f
		[{token = SBOpen}:_] = f
		[{token = Op Not}:_] = f
		[{token = Op Min}:_] = f
		[{token = POpen}:_] = f
		[{token = KTrue}:_] = f	
		[{token = KFalse}:_] = f
		[{token = Integer _}:_] = f
		[{token = Identifier _}:_] = f
		_ = { PR | result = e2 (Alg c []) xs, tokens = xs }	
	where f = let { PR | result = e, tokens = xe } = parseExp xs in { PR | result = e2 (Alg c [e]) xe, tokens = xe }
	parseFactorExp tokens=:[{token = POpen}:xs] = case e.PR.tokens of
		[{token = Comma}:xxs] = let ex = parseExp xxs in if ((hd ex.PR.tokens).token === PClose) (pr (e2 (Tup e.PR.result ex.PR.result) tokens) (tl ex.PR.tokens)) (parseError xxs "expecting )")
		[{token = PClose}:xss] = pr (e2 (EBrace e.PR.result) tokens) xss
		_ = parseError xs "expecting ) or ,"
	where e = parseExp xs
	parseFactorExp tokens=:[{token = Identifier name}:[{token = POpen}:xs]] = let args = parseFArgExps xs in 
											pr (e2 (EFC { FunCall | callName = name, callArgs = args.PR.result }) tokens) args.PR.tokens
	parseFactorExp tokens=:[{token = Identifier name}:xs] = pr (e2 (I name) tokens) xs
	parseFactorExp tokens = parseError tokens "expecting expression"
