module HTTP.Request where

-- GET /home/example HTTP/1.1
-- Host: site.com
-- Accept-Language: en

import Data.Char (isSpace, isDigit, isAlpha)
import Parser
import Control.Applicative (many, some, (<|>))
import qualified Utils as U
import HTTP.Common
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString as BS
import JSON.JParser

-- Add the rest later
data Method = POST | GET deriving Show

type QueryParameter = (BS.ByteString, BS.ByteString)

data Request = Request 
    {    getMethod  :: Method
    ,    getPath    :: Path
    ,    parameters :: [QueryParameter]
    ,    getVersion :: ProtocolVersion -- HTTP (major.minor) version
    ,    getHeaders :: [Header]
    ,    contents   :: Contents
    } deriving (Show)

parseMethod :: Parser Method
parseMethod = f <$> alternate (parseSeq "GET") (parseSeq "POST")
    where f "GET"   = GET
          f "POST"  = POST
          f x       = error ("parseSeq incorrectly parsed: " ++ x)

parsePath :: Parser Path
parsePath = f <$> some (
            parseChar '/' *> 
            parseOrDefault "" (parseSpan (\x -> (not . isSpace) x && x /= '/' && x /= '?')))
            where f = map (\xs -> BI.packChars $ '/' : xs)

-- TODO: Improve the pair parser
parseQueryParameters :: Parser [QueryParameter]
parseQueryParameters = (parseChar '?' *> pairs) <|> pure []
                       where
                       pair = (\k v -> (BI.packChars k, BI.packChars v))
                              <$> (parseSpan (\x -> isAlpha x && x /= '=') <* parseChar '=')
                              <*> parseSpan (\x -> isAlpha x && x /= '&')
                       pairs = parseByTrailingSeperator 
                               (parseChar '&')
                               pair

parseVersion :: Parser ProtocolVersion
parseVersion = parseSeq "HTTP/" *> 
               ((\a b -> (U.readInt a, U.readInt b)) 
               <$> parseSpan isDigit)
               <*> (parseChar '.' *> parseSpan isDigit)

parseHeader :: Parser Header
parseHeader = (newHeader 
              <$> parseSpan (\x -> isAlpha x || x == '-')) 
              <*> (parseChar ':' *> parseWhiteSpace *> parseSpan (not . U.isNewLine))

parseHeaders :: Parser [Header]
parseHeaders = many $ parseHeader <* parseWhiteSpace

parseContents :: Parser Contents
parseContents = (JSON <$> parseJson) <|> (Text <$> parseSpan (const True)) <|> (None <$ parseNothing)

parseRequest :: Parser Request
parseRequest = (\m p q v h c -> Request { getMethod = m, getPath = p, parameters = q, getVersion = v, getHeaders = h, contents = c})
               <$> (parseMethod <* parseWhiteSpace)
               <*> parsePath
               <*> (parseQueryParameters <* parseWhiteSpace)
               <*> (parseVersion <* parseWhiteSpace)
               <*> (parseHeaders <* parseWhiteSpace)
               <*> parseContents

