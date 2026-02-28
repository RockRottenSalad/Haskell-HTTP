module JSON.JParser where

import JSON.Common
import Parser
import Control.Applicative

parseJsonNull :: Parser Json
parseJsonNull = JNull <$ parseSeq "null"

parseJsonBool :: Parser Json
parseJsonBool = f <$> alternate (parseSeq "true") (parseSeq "false")
                where
                f "true"  = JBool True
                f "false" = JBool False
                f x       = error $ "Parser incorrectly parsed: " ++ x


parseStringLiteral :: Parser String
parseStringLiteral = parseChar '"' *> parseSpan (/='"') <* parseChar '"'

parseJsonCommaSeparator :: Parser Char
parseJsonCommaSeparator = parseWhiteSpace *> parseChar ',' <* parseWhiteSpace

-- TODO: Support escaping "
parseJsonString :: Parser Json
parseJsonString = JString <$> parseStringLiteral

parseJsonNumber :: Parser Json
parseJsonNumber = JNumber <$> parseDouble

parseJsonArray :: Parser Json
parseJsonArray = parseChar '[' *> parseWhiteSpace *> array <* parseWhiteSpace <* parseChar ']'
                 where
                 array = JArray <$> parseByTrailingSeperator 
                                    parseJsonCommaSeparator
                                    parseJson

parseJsonObject :: Parser Json
parseJsonObject = parseChar '{' *> parseWhiteSpace *> keyValuePairs <* parseWhiteSpace <* parseChar '}'
    where
    keyValuePairs = JObject <$> parseByTrailingSeperator parseJsonCommaSeparator keyValuePair 
    keyValuePair = (,) <$>
                   (parseWhiteSpace *> parseStringLiteral <* parseWhiteSpace <* parseChar ':')
                   <*> (parseWhiteSpace *> parseJson <* parseWhiteSpace)

parseJson :: Parser Json 
parseJson = parseJsonNull <|> parseJsonBool <|> parseJsonString <|> parseJsonNumber <|> parseJsonArray <|> parseJsonObject

