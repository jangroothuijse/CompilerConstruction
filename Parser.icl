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
parseDecl t
|		(hd t).Token.token === KVoid = let iden = parseId (tl t) in parseFunDecl TVoid iden.result iden.tokens
#		type = parseType t		
#		iden = parseId type.PR.tokens
|		(hd iden.tokens).Token.token === KAssign = let exp = parseExp (tl iden.PR.tokens) in let semi = parseSymbol Semicolon exp.PR.tokens in
			 { PR | tokens = semi.PR.tokens, result = V { VarDecl | type = type.result, name = iden.PR.result, exp = exp.PR.result } }
=		parseFunDecl (RT type.result) iden.result iden.tokens

parseSymbol :: Symbol [Token] -> PR Unit
parseSymbol t [{token = s}:xs] = { PR | result = Unit, tokens = xs }
parseSymbol t [x:xs] = parseError [x:xs] ("Expecting: " +++ (toString t) +++ " found: " +++ (toString x.Token.token))
parseSymbol _ _ = parseError [] ""

parseFunDecl :: RetType Id [Token] -> PR Decl
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

parseType :: [Token] -> PR Type
parseType [{token = KBool}:xs] = { PR | result = TBool, tokens = xs }
parseType [{token = KInt}:xs] = { PR | result = TInt, tokens = xs }
parseType [{token = (Identifier i)}:xs] = { PR | result = (TId i), tokens = xs }
parseType [{token = SBOpen}:xs] = let i = parseType xs in { PR | result = (TList i.PR.result), tokens = (parseSymbol SBClose i.PR.tokens).PR.tokens }
parseType [{token = POpen}:xs] = let t = ((parseType) ~># ((parseSymbol Comma) ~>- parseType)) xs in { PR | result = TTup t.PR.result, tokens = (parseSymbol SBClose t.PR.tokens).PR.tokens }
parseType t = parseError t " Failed to parse type"

propNothing :: (Maybe a) b -> Maybe b
propNothing x y = if (isNothing x) Nothing (Just y)

parseMaybeType :: [Token] -> Maybe (PR Type)	// Checks for a type without aborting
parseMaybeType [{token = KBool}:xs] = Just { PR | result = TBool, tokens = xs }
parseMaybeType [{token = KInt}:xs] = Just { PR | result = TInt, tokens = xs }
parseMaybeType [{token = (Identifier i)}:xs] = Just { PR | result = (TId i), tokens = xs }
parseMaybeType [{token = SBOpen}:xs] = let i = parseMaybeType xs in propNothing i { PR | result = TList (fromJust i).PR.result, tokens = (parseSymbol SBClose (fromJust i).PR.tokens).PR.tokens }
parseMaybeType [{token = POpen}:xs] = if (isJust t1 && split && isJust t2 && tupEnd) 
			(Just { PR | result = TTup ((fromJust t1).PR.result, (fromJust t2).PR.result), tokens = tl (fromJust t2).PR.tokens }) Nothing where
	t1 = parseMaybeType xs
	split = (hd (fromJust t1).PR.tokens).token === Comma
	t2 = parseMaybeType (tl (fromJust t1).PR.tokens)
	tupEnd = (hd (fromJust t1).PR.tokens).token === PClose
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

instance toString Symbol where toString s = abort "undef"

(~>) infixl 7 :: (Parse a) (Parse b) -> ([Token] -> PR a)			// Requires ab, returns a
(~>) a b = (\t -> let ra = a t in let rb = b ra.PR.tokens in { PR | ra & tokens = rb.PR.tokens })

(~>#) infixl 7 :: (Parse a) (Parse b) -> ([Token] -> PR (a, b))		// Requires ab, returns (a, b)
(~>#) a b = (\t -> let ra = a t in let rb = b ra.PR.tokens in { PR | result = (ra.PR.result, rb.PR.result), tokens = rb.PR.tokens } )

(~>-) infixl 7 :: (Parse a) (Parse b) -> ([Token] -> PR b)		// Requires ab, returns b
(~>-) a b = (\t -> let ra = a t in b ra.PR.tokens)

parseError :: [Token] String -> a
parseError [] e = abort ("PARSE ERROR: Unexpected end of file " +++ e)
parseError [x:xs] e = abort ("PARSE ERROR: " +++ e +++ " on line " +++ (toString x.Token.line) +++ " column " +++ (toString x.Token.column) +++ "\n")

parseExp :: [Token] -> PR Exp
parseExp t = let e = parseAndExp t in if (not ((hd e.PR.tokens).Token.token === (Op Cons))) (pr e.PR.result e.PR.tokens)
	(let ex = parseExp (tl e.PR.tokens) in { PR | result = e2 (Op2 e.PR.result PCons ex.PR.result) t, tokens = ex.PR.tokens }) // <- Cons right asso
where
	e2 e1 t = { Exp | ex = e1, eline = (hd t).Token.line, ecolumn = (hd t).Token.column }	
	f op2 tx e g = let ex = g tx in { PR | result = e2 (Op2 e.PR.result op2 ex.PR.result) t, tokens = ex.PR.tokens }
	parseAndExp t = let e = parseOrExp t in if (not ((hd e.PR.tokens).Token.token === (Op And))) (pr e.PR.result e.PR.tokens)
		(let ex = parseOrExp (tl e.PR.tokens) in { PR | result = e2 (Op2 e.PR.result PAnd ex.PR.result) t, tokens = ex.PR.tokens })	// <- Others left asso
	parseOrExp t = let e = parseRelExp t in if (not ((hd e.PR.tokens).Token.token === (Op Or))) (pr e.PR.result e.PR.tokens)
		(let ex = parseRelExp (tl e.PR.tokens) in { PR | result = e2 (Op2 e.PR.result POr ex.PR.result) t, tokens = ex.PR.tokens })
	parseRelExp t = case e.PR.tokens of 
		[{token = Op op}:tx] = case op of
			Eq	= f PEq tx e (parseSumExp)
			LT	= f PLT tx e (parseSumExp)
			GT	= f PGT tx e (parseSumExp)
			LTE = f PLTE tx e (parseSumExp)
			GTE = f PGTE tx e (parseSumExp)
			NEq = f PNEq tx e (parseSumExp)
			_	= (pr e.PR.result e.PR.tokens)
		t 	=	(pr e.PR.result e.PR.tokens)
	where e = parseSumExp t
	parseSumExp t = case e.PR.tokens of 
		[{token = Op op}:tx] = case op of
			Plus	= f PPlus tx e (parseTermExp)
			Min		= f PMin tx e (parseTermExp)
			_	= (pr e.PR.result e.PR.tokens)
		t 	=	(pr e.PR.result e.PR.tokens)
	where e = parseTermExp t
	parseTermExp t = case e.PR.tokens of 
		[{token = Op op}:tx] = case op of
			Mul		= f PMul tx e (parseFactorExp)
			Div		= f PDiv tx e (parseFactorExp)
			Mod		= f PMod tx e (parseFactorExp)
			_	= (pr e.PR.result e.PR.tokens)
		t 	=	(pr e.PR.result e.PR.tokens)
	where e = parseFactorExp t
	parseFactorExp tokens=:[{token = Op Not}:xs] = let e = parseExp xs in pr (e2 (Op1 PNot e.PR.result) tokens) e.PR.tokens
	parseFactorExp tokens=:[{token = Op Min}:xs] = let e = parseExp xs in pr (e2 (Op1 PNeg e.PR.result) tokens) e.PR.tokens
	parseFactorExp tokens=:[{token = Integer z}:xs] = pr (e2 (EInt z) tokens) xs
	parseFactorExp tokens=:[{token = KTrue}:xs] = pr (e2 ETrue tokens) xs
	parseFactorExp tokens=:[{token = KFalse}:xs] = pr (e2 EFalse tokens) xs
	parseFactorExp tokens=:[{token = SBOpen}:[{token = SBClose}:xs]] = pr (e2 EBlock tokens) xs
	parseFactorExp tokens=:[{token = POpen}:xs] = case e.PR.tokens of
		[{token = Comma}:xxs] = let ex = parseExp xxs in if ((hd ex.PR.tokens).token === PClose) (pr (e2 (Tup e.PR.result ex.PR.result) tokens) (tl ex.PR.tokens)) (parseError xxs "expecting )")
		[{token = PClose}:xss] = pr (e2 (EBrace e.PR.result) tokens) xss
		_ = parseError xs "expecting ) or ,"
	where e = parseExp xs
	parseFactorExp tokens=:[{token = Identifier name}:[{token = POpen}:xs]] = let args = parseFArgExps xs in 
											pr (e2 (EFC { FunCall | callName = name, callArgs = args.PR.result }) tokens) args.PR.tokens
	parseFactorExp tokens=:[{token = Identifier name}:xs] = pr (e2 (I name) tokens) xs
	parseFactorExp tokens = parseError tokens "expecting expression"
