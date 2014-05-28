module Cis194.Hw.Golf where

import Data.List (unfoldr)

pick' :: Int -> [a]-> [a]
pick' n   = drop (n-1). (take n)

nth :: Int -> [a] -> [a]
nth n [] = []
nth n xs = (pick' n xs) ++ nth n (drop n xs)

repl :: [a] -> [[a]]
repl xs = replicate (length xs) xs

skips :: [a] -> [[a]]
skips xs  = zipWith nth [1..] $ repl xs

maxx :: [Integer] -> [Integer] 
maxx xs  =  if (b > a)  && (b > c) then 
              [b]
            else 
              [] 
           where a:b:c:d = xs

chunks :: Int -> [a] -> [[a]]
chunks n xs = filter  (\ xs -> length xs > 2) $ takeWhile (not.null) $ unfoldr (Just . splitAt n) xs

--localMaxima :: [Integer] -> [Integer]
-- localMaxima  = map maxx

-- localMaxima xs = case xs of
--                  a:[] -> []
--                  a:b:[] -> []
--                  a:b:c:[] -> (take 3 xs)
--                  a:b:c:d:[] -> (take 3 xs) ++ localMaxima (drop 1 xs)
--                  _ -> []


listOfLists :: [Integer]->[[Integer]]
listOfLists xs = case (length xs >= 3) of
                 True -> [take 3 xs] ++ listOfLists (drop 1 xs)
                 _ ->  []

localMaxima :: [Integer] -> [Integer]
localMaxima  = concat . filter (not . null) . map maxx . listOfLists
                 

histogram :: [Integer] -> String
histogram _ = ""
