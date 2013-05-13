definition module AlphaNumIndexed

import DigitalTree

(getIndexed) infixr 5 :: *(DigitalTree a) (!String, a) -> (a, *(DigitalTree a))
(addIndexed) infixr 5 :: *(DigitalTree a) (!String, a)  -> *(DigitalTree a)
newIndexed :: *(DigitalTree a)
