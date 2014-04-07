{-# LANGUAGE OverloadedStrings #-}

import Data.Word
import Data.Time
import Data.Attoparsec.Char8 as A
import Control.Applicative
import qualified Data.ByteString as B

-- Types --
data IP = IP Word8 Word8 Word8 Word8 deriving Show

data Method = Get | Post deriving Show

data HTTPVersion = V10 | V11 deriving Show

data Scheme = HTTP | HTTPS deriving Show

data LogLine =
  LogLine  { ip :: IP
           , day :: Day
           , timeOfDay :: TimeOfDay
           , method :: Method
           , route :: B.ByteString
           , httpVersion :: HTTPVersion
           , statusCode :: Int
           , dunno :: Int
           , scheme :: Scheme
           , userAgent :: B.ByteString
             } deriving Show

type Log = [LogLine]

-- Parsers --
parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4

parseMonth :: Parser Int
parseMonth = do
      (string "Jan" >> return  1)
  <|> (string "Feb" >> return  2)
  <|> (string "Mar" >> return  3)
  <|> (string "Apr" >> return  4)
  <|> (string "May" >> return  5)
  <|> (string "Jun" >> return  6)
  <|> (string "Jul" >> return  7)
  <|> (string "Aug" >> return  8)
  <|> (string "Sep" >> return  9)
  <|> (string "Oct" >> return 10)
  <|> (string "Nov" >> return 11)
  <|> (string "Dec" >> return 12)

parseDay :: Parser Day
parseDay = do
  day <- count 2 digit
  char '/'
  month <- parseMonth
  char '/'
  year <- count 4 digit
  return $ fromGregorian (read year) (month) (read day)

parseTimeOfDay :: Parser TimeOfDay
parseTimeOfDay = do
  hour <- count 2 digit
  char ':'
  minute <- count 2 digit
  char ':'
  second <- count 2 digit
  return $ TimeOfDay (read hour) (read minute) (read second)

parseMethod :: Parser Method
parseMethod = do
      (string "GET" >> return Get)
  <|> (string "POST" >> return Post)

parseHTTPVersion :: Parser HTTPVersion
parseHTTPVersion = do
      (string "1.0" >> return V10)
  <|> (string "1.1" >> return V11)

parseScheme :: Parser Scheme
parseScheme = do
      (string "http" >> return HTTP)
  <|> (string "https" >> return HTTPS)

parseLogLine = do
  ip <- parseIP
  string " - - ["
  day <- parseDay
  char ':'
  timeOfDay <- parseTimeOfDay
  string " +0000" -- ignore timezone
  char ']'
  char ' '
  char '"'
  method <- parseMethod
  char ' '
  route <- A.takeWhile (/=' ')
  string " HTTP/"
  version <- parseHTTPVersion
  char '"'
  char ' '
  statusCode <- count 3 digit
  char ' '
  dunno <- count 4 digit
  char ' '
  char '"'
  scheme <- parseScheme
  string "://datahub.io/" -- host
  char '"'
  char ' '
  char '"'
  userAgent <- A.takeWhile (/='"')
  char '"'
  return $
    LogLine  { ip = ip
             , day = day
             , timeOfDay = timeOfDay
             , method = method
             , route = route
             , httpVersion = version
             , statusCode = (read statusCode)
             , dunno = (read dunno)
             , scheme = scheme
             , userAgent = userAgent
               }

parseLog :: Parser Log
parseLog = many $ parseLogLine <* endOfLine

--logFile = "/home/tlevine/safe/datahub-log-analysis/nginx/access.log"
logFile = "/tmp/access.log"

main :: IO ()
main = do
  file <- B.readFile logFile
  let r = parseOnly parseLog file
  case r of
    Left err -> putStrLn $ "A parsing error was found: " ++ err
    Right log -> putStrLn $ show log
