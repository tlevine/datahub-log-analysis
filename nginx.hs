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

{-
data Date =

data Method = GET | POST deriving Show

Route
data HTTPVersion = V10 | V11

data LogEntry =
  LogEntry { entryTime :: LocalTime
           , entryIP   :: IP
           , entryProduct   :: Product
           } deriving Show
-}

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

main :: IO ()                                                                                                                                                          
main = print $ parseOnly parseIP "23.27.112.125 - - [02/Apr/2014:12:27:57 +0000] \"GET /user/register HTTP/1.0\" 200 4285 \"http://datahub.io/\" \"Mozilla/5.0 (Windows NT 5.1; rv:13.0) Gecko/20100101 Firefox/13.0.1\""
