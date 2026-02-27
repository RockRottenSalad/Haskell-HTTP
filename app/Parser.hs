{-# LANGUAGE LambdaCase #-}
module Parser where

import Control.Monad
import Control.Applicative
import Data.Char (isSpace)
import Data.Functor

-- Either with parser error would be better than a maybe, but this will have to do for now
newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser g) =
            Parser $ g >=> \(y, x') -> return (f y, x')

instance Applicative Parser where
    pure y = Parser $ \x -> Just (y, x)
    (Parser f) <*> (Parser g) =
        Parser $ \x -> do
        (h, x') <- f x
        (y, x'') <- g x'
        return (h y, x'')

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

parseOrDefault :: a -> Parser a -> Parser a
parseOrDefault defaultValue (Parser f) = Parser $ \x -> do
                                        case f x of
                                            Just v -> Just v
                                            Nothing -> Just (defaultValue, x)

-- Leading seperator means the sep comes before the element, i.e. ,4,1,2
-- or, more realistically: /home/user/directory/example
-- BUG: /home/ <- last / is not parsed, tmp fix has been placed
-- Bounded constraint is a placeholder until better solution is found for the aforementioned bug 
parseByLeadingSeperator :: Bounded a => Parser a -> Parser b -> Parser [b]
parseByLeadingSeperator seperator element = seperator *> some (element <* parseOrDefault minBound seperator) <|> seperator $> []

