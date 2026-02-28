module HTTP.Request where

-- GET /home/example HTTP/1.1
-- Host: site.com
-- Accept-Language: en

import Data.Char (isSpace, isDigit)
import Parser
import Control.Applicative (many, some)
import qualified Utils as U
import HTTP.Common
import qualified Data.ByteString.Internal as BI

-- Add the rest later
data Method = POST | GET deriving Show

data Request = Request 
    {    getMethod  :: Method
    ,    getPath    :: Path
    ,    getVersion :: ProtocolVersion -- HTTP (major.minor) version
    ,    getHeaders :: [Header]
    } deriving (Show)

parseMethod :: Parser Method
parseMethod = f <$> alternate (parseSeq "GET") (parseSeq "POST")
    where f "GET"   = GET
          f "POST"  = POST
          f x       = error ("parseSeq incorrectly parsed: " ++ x)

-- BUG: What about /path/ <-- Last / fails to parse
-- Added tmp fix in parseByLeadingSeperator
--parsePath :: Parser Path
--parsePath = f <$> parseByLeadingSeperator 
--                (parseChar '/')
--                (parseSpan (\x -> (not . isSpace) x && x /= '/'))
--            where f = map BI.packChars

parsePath :: Parser Path
parsePath = f <$> some (
            parseChar '/' *> 
            parseOrDefault "" (parseSpan (\x -> (not . isSpace) x && x /= '/')))
            where f = map (\xs -> BI.packChars $ '/' : xs)

parseVersion :: Parser ProtocolVersion
parseVersion = parseSeq "HTTP/" *> 
               ((\a b -> (U.readInt a, U.readInt b)) 
               <$> parseSpan isDigit)
               <*> (parseChar '.' *> parseSpan isDigit)

parseHeader :: Parser Header
parseHeader = (newHeader 
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

