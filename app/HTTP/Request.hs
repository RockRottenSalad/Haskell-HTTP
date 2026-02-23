module HTTP.Request where

-- GET /home/example HTTP/1.1
-- Host: site.com
-- Accept-Language: en

import Data.Char (isSpace, isDigit)
import Parser
import Control.Applicative (many)
import qualified Utils as U

-- Add the rest later
data Method = POST | GET deriving Show
type Path = [String]

data Header = Header
    {    getKey     :: String
    ,    getValue   :: String 
    } deriving (Show)

data Request = Request 
    {    getMethod  :: Method
    ,    getPath    :: Path
    ,    getVersion :: (Int, Int) -- HTTP (major.minor) version
    ,    getHeaders :: [Header]
    } deriving (Show)

parseMethod :: Parser Method
parseMethod = f <$> alternate (parseSeq "GET") (parseSeq "POST")
    where f "GET"   = GET
          f "POST"  = POST
          f _       = error "Parser is cooked"

parsePath :: Parser Path
parsePath = parseByLeadingSeperator 
                (parseChar '/')
                (parseSpan (\x -> (not . isSpace) x && x /= '/'))

parseVersion :: Parser (Int, Int)
parseVersion = parseSeq "HTTP/" *> 
               ((\a b -> (U.readInt a, U.readInt b)) 
               <$> parseSpan isDigit)
               <*> (parseChar '.' *> parseSpan isDigit)

-- Normalise newlines?
-- Something better? idk
parseHeader :: Parser Header
parseHeader = ((\k v -> Header { getKey = k, getValue = v }) 
              <$> parseSpan (/=':')) 
              <*> (parseChar ':' *> parseWhiteSpace *> parseSpan (not . U.isNewLine))

parseHeaders :: Parser [Header]
parseHeaders = many $ parseHeader <* parseWhiteSpace

parseRequest :: Parser Request
parseRequest = (\m p v h -> Request { getMethod = m, getPath = p, getVersion = v, getHeaders = h })
               <$> (parseMethod <* parseWhiteSpace)
               <*> (parsePath <* parseWhiteSpace)
               <*> (parseVersion <* parseWhiteSpace)
               <*> parseHeaders

