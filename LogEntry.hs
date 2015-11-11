import Data.Time.Clock
import Data.Time.Format
import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"
logLevelToString _ = "Info"

logEntryToString :: LogEntry -> String
logEntryToString le = (timeToString . timestamp $ le) ++ ": " ++ (logLevelToString . logLevel $ le) ++ ": " ++ (message le)