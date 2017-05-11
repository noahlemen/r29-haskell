{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = parseWords (words s)

parseWords :: [String] -> LogMessage
parseWords ("I":n:s)   = LogMessage Info (read n) (unwords s)
parseWords ("W":n:s)   = LogMessage Warning (read n) (unwords s)
parseWords ("E":e:n:s) = LogMessage (Error (read e)) (read n) (unwords s)
parseWords (s)         = Unknown (unwords s)

parse :: String -> [LogMessage]
parse (s) = [parseMessage l | l <- lines s]
