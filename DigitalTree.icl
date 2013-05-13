implementation module DigitalTree

import StdEnv

:: DT a = DTEmpty | DTTail String a | DTNode .{ DT a } | DTFullNode a .{ DT a } | DTLeaf a
:: DigitalTree a = { data :: .(DT a), indexSize :: Int }

newTree :: Int -> *(DigitalTree a)
newTree i = { DigitalTree | data = DTEmpty, indexSize = i }

toNumbers :: String -> String
toNumbers s = foldl (+++) "" [ ((toString o toInt) c) +++ " " \\ c <-: s ]

stringFrom :: !String !Int -> String
stringFrom s i = { s.[j] \\ j <- [i..(size s-1)] }

isValidKey i s = foldr (&&) True [let x = toInt y in x >= 0 && x < i \\ y <-: s]

(add) infixr 5 :: *(DigitalTree a) (!String, a)  -> *(DigitalTree a)
(add) old=:{ indexSize = i, data = data } (key, value)
	| not (isValidKey i key) = abort ("Invalid key while adding: " +++ key +++ " - " +++ (toNumbers key) +++ "\n")
	= { DigitalTree | indexSize = i, data = g data key value 0 } where 
	//g :: *(DT a) String a !Int -> *(DT a)
	g (DTLeaf xVal) key val offset
		| size key <= offset = DTLeaf val
		# x = (toInt key.[offset])
		# newArray = { DTEmpty \\ j <- [0..i-1] } 
		= DTFullNode xVal { newArray & [x] = g DTEmpty key val (offset + 1) }	
	g DTEmpty key val offset
		//= DTEmpty
		| offset >= size key = DTLeaf val
		= DTTail (stringFrom key offset) val
	g (DTTail xKey xVal) key val offset // Tails are never empty, keys could be
		# newArray = { DTEmpty \\ j <- [0..i-1] }
		# x = toInt xKey.[0]
		# (elem, newArray) = newArray![x]
		# newArray = { newArray & [x] = g elem xKey xVal 1 }
		| size key <= offset = DTFullNode val newArray
		# y = toInt key.[offset]
		# (elem2, newArray) = newArray![y]
		= DTNode { newArray & [y] = g elem2 key val (offset + 1) }
		//= DTFullNode val newArray // Fuck logic!!! Something goes wrong, perhaps use simpler examples to pinpoint the horror
	g (DTNode arr) key val offset
		| size key <= offset = DTFullNode val arr
		# x = toInt key.[offset]
		# (elem, arr) = arr![x]
		= DTNode { arr & [x] = g elem key val (offset + 1) }
	g (DTFullNode nVal arr) key val offset
		| size key <= offset = DTFullNode val arr
		# x = toInt key.[offset]
		# (elem, arr) = arr![x]
		= DTFullNode nVal { arr & [x] = g elem key val (offset + 1) }

(get) infixr 5 :: *(DigitalTree a) (!String, a) -> (a, *(DigitalTree a))
(get) tree=:{ indexSize = i, data = data } (key, default)
	| not (isValidKey i key) = abort ("Invalid key while getting: " +++ key +++ " - " +++ (toNumbers key) +++ "\n")
	= (v, { DigitalTree | tree & data = newData }) where
	( v, newData ) = f data 0
	//f :: E.b: *(DT b) !Int -> (b, *DT b)
	f DTEmpty offset = (default, DTEmpty)
	f dt=:(DTLeaf val) offset = if (sizeKey <= offset) (val, dt) (default, dt)
	f dt=:(DTTail tail val) offset = if ((stringFrom key offset) == tail) (val, dt) (default, dt)
	f (DTNode arr) offset 
		| sizeKey <= offset = (default, DTNode arr)
		# index = toInt key.[offset]
		# (element, arr) = arr![index]
		# (val, element) = f element (offset + 1)
		= (val, DTNode { arr & [index] = element } )
	f dt=:(DTFullNode val arr) offset 
		| sizeKey <= offset = (val, dt)
		# index = toInt key.[offset]
//		= abort ("Index = " +++ (toString index) +++ " offset = " +++ (toString offset) +++ " sizeKey = " +++ (toString sizeKey) +++ "\n")
		# (element, arr) = arr![index]	
		# (rval, element) = f element (offset + 1)
		= (rval, DTFullNode val { arr & [index] = element })
	sizeKey = size key

Start
#	tree = newTree 256
#	tree = tree add ("aa1", 1001)
#	tree = tree add ("aa1111", 1000)
= fst (tree get ("aa1", (abort "No such element available\n")))
