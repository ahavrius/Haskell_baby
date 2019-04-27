sumInts::Int->Int->Int
--sumInts a b = min a b  
--sumInts a b = foldl1 (\x y -> x + y) [min a b .. max a b ]
--sumInts a b = foldl (+) 0 [min a b .. max a b ]
sumSquares::Int->Int->Int
--sumSquares a b = foldl (+) 0 $ map (\x -> x*x) [min a b .. max a b ]
--sumSquares a b = foldl (+) 0 $ map Sq [min a b .. max a b ]
higherOrderSum::(Int->Int)->Int->Int->Int
higherOrderSum f a b = foldl (+) 0 $ map f [min a b .. max a b]
sumInts a b = higherOrderSum id a b 
--sumInts a b = higherOrderSum (+0) a b 
sumSquares a b = higherOrderSum (^2) a b 
higherOrderSequenceApplication:: Int->Int->(Int->Int)->(Int->Int->Int)->Int
higherOrderSequenceApplication a b f g = foldl1 g $ map f [min a b .. min a b]
factorial n = higherOrderSequenceApplication 1 n id (*)