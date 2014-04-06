{-# LANGUAGE OverloadedStrings #-}

import Data.Word
import Data.Time
import Data.Attoparsec.Char8
import Control.Applicative

-----------------------
-------- TYPES --------
-----------------------

-- | Type for IP's.
data IP = IP Word8 Word8 Word8 Word8 deriving Show

data Method = GET | POST deriving Show

data HTTPVersion = V10 | V11 deriving Show

-- | Parser of values of type 'IP'.
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

parseLogEntry = do
  ip <- parseIP
  string " - - ["
  day <- parseDay
  return day

main :: IO ()
main = print $ parseOnly parseLogEntry "23.27.112.125 - - [02/Apr/2014:12:27:57 +0000] \"GET /user/register HTTP/1.0\" 200 4285 \"http://datahub.io/\" \"Mozilla/5.0 (Windows NT 5.1; rv:13.0) Gecko/20100101 Firefox/13.0.1\""
