{-# LANGUAGE LambdaCase #-}
module Parser where

--import Data.Maybe
import Control.Monad
import Control.Applicative
import Data.Char (isSpace)
import Data.Functor

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser g) = do
        Parser $ g >=> \(y, x') -> return (f y, x')

instance Applicative Parser where
    pure y = Parser $ \x -> Just (y, x)
    (Parser f) <*> (Parser g) =
        Parser $ \x -> do
        (h, x') <- f x
        (y', x'') <- g x'
        return (h y', x'')

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser f) <|> (Parser g) = Parser $ \x -> f x <|> g x

alternate :: Parser a -> Parser a -> Parser a
alternate a b = a <|> b

combinatoricParser :: [Parser a] -> Parser a
combinatoricParser [] = Parser $ const Nothing
combinatoricParser ps = foldl1 (<|>) ps

parseCharBy :: (Char -> Bool) -> Parser Char
parseCharBy predicate = Parser $ \case (y : ys) | predicate y -> Just (y, ys)
                                       _ -> Nothing

parseChar :: Char -> Parser Char
parseChar expect = parseCharBy (==expect)

parseSeq :: String -> Parser String
parseSeq expect = Parser $ \x -> let
                                 (matched, x') = splitAt (length expect) x
                                 in if expect == matched
                                 then Just (matched, x')
                                 else Nothing

parseSpan :: (Char -> Bool) -> Parser String
parseSpan predicate = Parser $ \x -> let
                                (matched, remaining) = span predicate x
                                in if null x || (not . predicate) (head x)
                                then Nothing
                                else Just (matched, remaining)

parseWhiteSpace :: Parser String
parseWhiteSpace = parseSpan isSpace

-- Leading seperator means the sep comes before the element, i.e. ,4,1,2
-- or, more realistically: /home/user/directory/example
parseByLeadingSeperator :: Parser a -> Parser b -> Parser [b]
parseByLeadingSeperator seperator element = some (seperator *> element) <|> seperator $> []

