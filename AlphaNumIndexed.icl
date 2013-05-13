implementation module AlphaNumIndexed

import StdEnv, DigitalTree

// Numbers 0 to 9, ALPHA 10 to 35, alpha 36 to 61, _ 62
toIndex :: String -> String
toIndex s
| foldl (||) False [True \\ e <-: s, d <- [toInt e] | (not ((d > 47 && d < 58) || (d > 64 && d < 91) || (d > 96 && d < 123) || d == 95))] = abort ("Trying to index key: " +++ s +++ "\n")
= { let j = let i = toInt x in if (i > 47 && i < 58) (i - 48) (if (i > 64 && i < 91) (i - 55) (if (i == 95) 62 (i - 61))) in toChar j \\ x <-: s }

(getIndexed) infixr 5 :: *(DigitalTree a) (!String, a) -> (a, *(DigitalTree a))
(getIndexed) t (t1, t2) = t get (toIndex t1, t2)

(addIndexed) infixr 5 :: *(DigitalTree a) (!String, a)  -> *(DigitalTree a)
(addIndexed) t (t1, t2) = t add (toIndex t1, t2)

newIndexed :: *(DigitalTree a)
newIndexed = newTree 63

Start
# t = newIndexed
# t = t addIndexed ("id0", 1)
//= t
# t = t addIndexed ("id01", 2)
= fst (t getIndexed ("id01", 3))
