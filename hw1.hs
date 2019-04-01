--card validation

toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0   = []
    | x < 10   = [x]
    |otherwise = toDigits (x `div` 10) ++ [x `mod` 10]
--toDigits x = reverse toDigitsRev --may be faster

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0   = []
    | x < 10   = [x]
    |otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ys =
    let     
        doubleEvery [] = []
        doubleEvery [a] = [a]
        doubleEvery (x:y:xs) = x:(2*y):doubleEvery xs
    in reverse (doubleEvery (reverse ys))
sumDigits :: [Integer] -> Integer
sumDigits ls = sum [sum(toDigitsRev x) | x<- ls]
--sumDigits ls = sum [sum(toDigitsRev x)| x<-doubleEveryOther ls ]
validate :: Integer -> Bool
validate x =  summ `mod` 10 == 0
    where ls = toDigits x
          double = doubleEveryOther ls
          summ = sumDigits double

--hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n <= 0   = error "Incorrect number"
    | n == 1   = [(a, b)]
    |otherwise = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi2 n a b c d
    | n <= 0   = error "Incorrect number"
    | n == 1   = [(a, b)]
    | n == 2   = [(a, c), (a, b), (c, b)]
    | n == 3   = [(a, c), (a, d), (a, b), (d, b), (c, b)]
--362
--   |otherwise = hanoi2 (n - (n `div` 2)) a c b d ++ hanoi (n `div` 2) a d b ++ hanoi (n `div` 2) d b a ++ hanoi2 (n - (n `div` 2)) c b d a
--305
    |otherwise = hanoi2 (n `div` 2) a c b d ++ hanoi (n - (n `div` 2)-1) a d b ++ [(a, b)] ++ hanoi (n - (n `div` 2)-1) d b a ++ hanoi2 (n `div` 2) c b d a
--    |otherwise = hanoi2 (n `div` 2) a b c d ++ hanoi2 (n - (n `div` 2)-1) a b d c ++ [(a, b)] ++ hanoi2 (n `div` 2) c b a d ++ hanoi2 (n - (n `div` 2)-1) d b a c
--    |otherwise = hanoi (n `div` 2) a c b ++ hanoi (n - (n `div` 2)-1) a d b ++ [(a, b)] ++ hanoi (n - (n `div` 2)-1) d b a ++ hanoi (n `div` 2) c b a
