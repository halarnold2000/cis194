module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]

--toDigits 0 = []
--toDigits x = if (x < 0) then [] else 
--             let last = last' x
--                 allbut = allButLast x
--             in (toDigits allbut)++ [last]

--toDigits n = if (n <= 0) then  [] 
--             else
--               let last = last' n
--                   allbut = allButLast n
--               in (toDigits allbut) ++ [last]          

toDigits n | n <= 0 = []
           | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]  

-- pick off last digit
last' :: Integer -> Integer
last' x = x `mod` 10

-- allButLast 
allButLast :: Integer -> Integer
allButLast x = x `div` 10 

toDigitsRev :: Integer -> [Integer]
-- toDigitsRev = reverse . toDigits 

toDigitsRev n | n <= 0 = []
              | otherwise =  [last' n] ++ toDigitsRev  (allButLast n)

doubleEveryOther :: [Integer]-> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther all@(_:_:_) = let x = reverse all
                               in reverse (doubleEveryOther' x) 


doubleEveryOther' :: [Integer] -> [Integer]
--doubleEveryOther' [] = []
--doubleEveryOther' (x:[]) = [x]
doubleEveryOther' (x:y:zx) = [x, 2*y] ++ doubleEveryOther' zx

sumDigits :: [Integer] -> Integer
sumDigits _ = 0

validate :: Integer -> Bool
validate _ = False

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi _ _ _ _ = []

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = []
