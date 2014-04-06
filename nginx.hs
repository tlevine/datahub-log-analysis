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

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving Show

-- data Date =

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

parseMonth :: Parser Month
parseMonth = do
      (string "Jan" >> return Jan)
  <|> (string "Feb" >> return Feb)
  <|> (string "Mar" >> return Mar)
  <|> (string "Apr" >> return Apr)
  <|> (string "May" >> return May)
  <|> (string "Jun" >> return Jun)
  <|> (string "Jul" >> return Jul)
  <|> (string "Aug" >> return Aug)
  <|> (string "Sep" >> return Sep)
  <|> (string "Oct" >> return Oct)
  <|> (string "Nov" >> return Nov)
  <|> (string "Dec" >> return Dec)


main :: IO ()
main = print $ parseOnly parseMonth "Apr"
--main = print $ parseOnly parseIP "23.27.112.125 - - [02/Apr/2014:12:27:57 +0000] \"GET /user/register HTTP/1.0\" 200 4285 \"http://datahub.io/\" \"Mozilla/5.0 (Windows NT 5.1; rv:13.0) Gecko/20100101 Firefox/13.0.1\""
