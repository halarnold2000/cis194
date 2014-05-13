{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc
import  Cis194.Hw.Log

parseMessage :: String -> LogMessage
parseMessage s = case s of
  [] -> Unknown "not good"
  'E':_ -> do 
              let  _:y:z:zs = (words s)
                   er = (read y)::Int
                   ts = (read z)::Int
                   str= unwords zs      
              LogMessage (Error er) ts str
  'I':_ -> do 
              let  _:z:zs = (words s)      
                   ts = (read z)::Int
                   str= unwords zs
              LogMessage Info ts str
  'W':_ -> do
              let  _:z:zs = (words s)
                   ts = (read z)::Int
                   str= unwords zs
              LogMessage Warning ts str 
  _ -> Unknown s

parse :: String -> [LogMessage]
parse s = case s of
   "" -> []
   all@(x:xs) -> let dems = lines all
                 in map parseMessage dems


insert :: LogMessage -> MessageTree -> MessageTree
insert LogMessage Leaf = Node Leaf LogMessage Leaf
insert _ t = t

build :: [LogMessage] -> MessageTree
build _ = Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder _ = []

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong _ = []
