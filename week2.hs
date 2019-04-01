data Colour = Red | Orange | Yellow | Green | Blue | Indigo | Violet
    deriving (Eq, Ord, Show, Bounded, Enum)

reverseColourOrder :: (Enum a, Bounded a) => [a]
reverseColourOrder = reverse [x| x<-[minBound .. maxBound]]
--```mixColours c1 c2 = _```
center a = a !! (length a `div` 2)
mixColours :: Colour -> Colour -> Colour
mixColours c1 c2 = center [min c1 c2 ..max c1 c2]

divTuple :: (Eq a, Fractional a) => (a, a) -> a
--divTuple (a, b)
--    | b == 0     = error "0"
--    | otherwise  = a / b
divTuple (_, 0) = error "0"
divTuple (a, b) =  a/b

threeZeroList :: [Int] -> Bool
threeZeroList (0:0:0:_) = True
threeZeroList _ = False