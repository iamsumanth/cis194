{-# OPTIONS_GHC -Wall #-}
module Spring13.Week2.LogAnalysis
  ( parse
  ) where

import Spring13.Week2.Log
import Data.List


parse:: String -> [LogMessage]
parse logs = map parseMessage (lines logs)

parseMessage:: String -> LogMessage
parseMessage message = constructMessage (getWords message)


constructMessage:: [String] -> LogMessage
constructMessage ("I":rest) = parseInfo rest
constructMessage ("W":rest) = parseWarning rest
constructMessage ("E":rest) = parseError rest
constructMessage unknownTypeMessage = Unknown (intercalate " " unknownTypeMessage)


parseInfo:: [String] -> LogMessage
parseInfo (timestamp:text) = LogMessage (Info) (read timestamp :: TimeStamp ) (intercalate " " text)
parseInfo unknownTypeMessage = Unknown (intercalate " " unknownTypeMessage)

parseWarning:: [String] -> LogMessage
parseWarning (timestamp:text) = LogMessage (Warning) (read timestamp :: TimeStamp ) (intercalate " " text)
parseWarning unknownTypeMessage = Unknown (intercalate " " unknownTypeMessage)

parseError:: [String] -> LogMessage
parseError (errorSeverity:timestamp:text) = LogMessage (Error (read errorSeverity :: Int )) (read timestamp :: TimeStamp ) (intercalate " " text)
parseError unknownTypeMessage = Unknown (intercalate " " unknownTypeMessage)


getWords :: String -> [String]
getWords line = words line

generateTree :: [String] -> MessageTree
generateTree messages = foldl (\tree message -> Spring13.Week2.LogAnalysis.insert (parseMessage message) tree) Leaf messages

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert newLog Leaf = Node Leaf newLog Leaf
insert newLog (Node leftTree currentLog rightTree) = if isLessThan newLog currentLog then Node (Spring13.Week2.LogAnalysis.insert newLog leftTree) (currentLog) (rightTree) else Node (leftTree) (currentLog) (Spring13.Week2.LogAnalysis.insert newLog rightTree)

isLessThan :: LogMessage -> LogMessage -> Bool
isLessThan (LogMessage _ newTimestamp _) (LogMessage _ oldTimestamp _) = newTimestamp < oldTimestamp