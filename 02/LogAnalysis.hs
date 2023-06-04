{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
  ("I" : ts : xs) -> LogMessage Info (read ts) (unwords xs) -- ts = timestamp
  ("W" : ts : xs) -> LogMessage Warning (read ts) (unwords xs)
  ("E" : lvl : ts : xs) -> LogMessage (Error (read lvl)) (read ts) (unwords xs)
  _ -> Unknown msg

parse :: String -> [LogMessage]
parse logs = map parseMessage (lines logs)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ ts1 _) (Node tree1 msg2@(LogMessage _ ts2 _) tree2)
  | ts1 < ts2 = Node (insert msg1 tree1) msg2 tree2
  | ts1 > ts2 = Node tree1 msg2 (insert msg1 tree2)
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

{- Original Solution (above recommended by linter)
build msgs = buildWithMessageTree msgs Leaf

buildWithMessageTree :: [LogMessage] -> MessageTree -> MessageTree
buildWithMessageTree [] tree = tree
buildWithMessageTree (msg : msgs) tree =
  buildWithMessageTree msgs (insert msg tree) -}

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node tree1 msg tree2) = inOrder tree1 ++ [msg] ++ inOrder tree2

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . inOrder . build . filter isSeverError

isSeverError :: LogMessage -> Bool
isSeverError (LogMessage (Error n) _ _) = n >= 50
isSeverError _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ m) = m
getMessage _ = ""