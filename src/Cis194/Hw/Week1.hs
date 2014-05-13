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

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther all@(x:y:zs)
  | isEven (length' all) = [2*x, y] ++ doubleEveryOther zs 
  | otherwise = x : [y*2, head zs] ++ doubleEveryOther (tail zs) 

bigInt :: Int -> Integer
bigInt = toInteger

length' :: [Integer]->Integer
length' = bigInt . length 

isEven :: Integer -> Bool
isEven = (== 0) . (`mod` 2)

sumDigits :: [Integer] -> Integer
--sumDigits _ = 0
sumDigits = sum . concat . map toDigits

doubleDigits :: Integer -> [Integer] 
doubleDigits = doubleEveryOther . toDigits

validate :: Integer -> Bool
validate  = isEven . sumDigits . concat . map toDigits . doubleDigits
--validate _  = False

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi _ _ _ _ = []

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = []
