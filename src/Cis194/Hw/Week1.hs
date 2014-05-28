module Cis194.Hw.Week1 where
import Data.Char

-------------
-- Ex 1-4  --
-------------


digitToInteger :: Char->Integer
digitToInteger = toInteger . digitToInt

toDigits :: Integer -> [Integer]

-- | this is some comment
-- >>> toDigits 1234
-- [1,2,3,4]

toDigits n 
           | n <= 0 = []
           | n > 0  = map digitToInteger $ (show n)

-- pick off last digit
last' :: Integer -> Integer
last' = flip mod 10

-- allButLast 
allButLast :: Integer -> Integer
allButLast = flip div 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- | doublEveryOther:
-- >>>  doubleEveryOther [8,7,6,5] 
-- [16,7,12,5]
-- >>> doubleEveryOther [1,2,3] 
-- [1,4,3] 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n  = reverse (zipWith (*) (cycle [1,2]) (reverse n))

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
