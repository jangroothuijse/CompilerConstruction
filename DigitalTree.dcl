definition module DigitalTree

:: DigitalTree a
newTree :: Int -> *(DigitalTree a)
(add) infixr 5 :: *(DigitalTree a) (!String, a)  -> *(DigitalTree a)
(get) infixr 5 :: *(DigitalTree a) (!String, a) -> (a, *(DigitalTree a))
