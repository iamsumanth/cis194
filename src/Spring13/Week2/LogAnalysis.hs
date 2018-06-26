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
