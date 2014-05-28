{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc
import Cis194.Hw.Log

-- $setup
-- >>> let foo = LogMessage Warning 10 "foo"
-- >>> let baz = LogMessage Warning 5 "baz"
-- >>> let bif = LogMessage Warning 15 "bif"
-- >>> let zor = LogMessage (Error 2) 562 "zor"

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
   xs -> let dems = lines xs
                 in map parseMessage dems

-- | 1. test for getter: ts
-- >>>  let foo = LogMessage Warning 10 "foo"
-- >>> tst foo
-- 10

tst :: LogMessage -> Int
tst (LogMessage _ t _) = t
tst _ = 0

-- | find LogMessage type
-- >>> let zor = LogMessage (Error 2) 562 "zor"
-- >>> logMessage zor

-- Error 2

-- logMessage :: LogMessage -> MessageType
-- logMessage (LogMessage l _  _) = l
-- logMessage _ = Info

-- | insert for LogMessages

-- >>> insert (Unknown "foo") Leaf 
-- Leaf

-- >>> let a = Leaf
-- >>> let b = LogMessage Warning 5 "baz"
-- >>> insert b a
-- Node Leaf b Leaf

-- | insert maintains the sort order of messages in the tree
-- >>> let foo = LogMessage Warning 10 "foo"
-- >>> let baz = LogMessage Warning 5 "baz" 
-- >>> let bif = LogMessage Warning 15 "bif"
-- >>> let zor = LogMessage (Error 2) 562 "zor"
    
-- >>> let a = Node Leaf foo Leaf
-- >>> insert baz a
-- Node (Node Leaf baz Leaf) foo Leaf
-- >>> insert bif b
-- Node (Node Leaf baz Leaf) foo1 (Node Leaf bif Leaf)

     
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _)  Leaf = Leaf
insert lm Leaf = Node Leaf lm Leaf
insert lm (Node l m r) | (tst lm) >= (tst m)  = Node l m (insert lm r)  
                       | otherwise = Node (insert lm l) m r

-- | Build: builds a MessageTree from a list of LogMessages
----- >>> let foo = LogMessage Warning 10 "foo"
---- >>> let baz = LogMessage Warning 5 "baz"
---- >>> let bif = LogMessage Warning 15 "bif"
-- >>> let ans = Node (Node Leaf (LogMessage Warning 5 "baz") Leaf) (LogMessage Warning 10 "foo") (Node Leaf (LogMessage Warning 15 "bif") Leaf)
-- >>> build [foo, baz, bif]
-- ans

----- Node (Node Leaf (LogMessage Warning 5 "baz") Leaf) (LogMessage Warning 10 "foo") (Node Leaf (LogMessage Warning 15 "bif") Leaf)

build :: [LogMessage] -> MessageTree
build  = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf  = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

-- | build then order:

buildThenOrder :: [LogMessage] -> [LogMessage]
buildThenOrder = inOrder . build



-- | function isError
isError :: MessageType -> Bool
isError x = case x of
             (Error _) -> True
             Warning -> False
             Info -> False 


-- | message type from log massage

messType :: LogMessage -> MessageType
messType (LogMessage mt _ _) = mt
messType _ = Info

-- | gets the message

mess :: LogMessage -> String
mess (LogMessage _ _ str) = str
mess _ = ""

-- | is this an error LogMessage?

isLogMessageError :: LogMessage -> Bool
isLogMessageError = isError . messType

-- | does this error message exceed 50?

exceedsThreshold50 :: MessageType -> Bool
exceedsThreshold50 (Error x)
                     | x >= 50 = True
                     | otherwise  = False
exceedsThreshold50 Info = False
exceedsThreshold50 Warning = False

thresholdFor50 :: LogMessage -> Bool
thresholdFor50 = exceedsThreshold50 . messType


-- let yy = filter isLogMessageError (buildThenOrder messages) 
                     
-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map mess $ filter thresholdFor50 $ filter isLogMessageError $ buildThenOrder xs
