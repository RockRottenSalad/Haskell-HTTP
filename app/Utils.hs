module Utils where

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

readInt :: String -> Int
readInt = read

isNewLine :: Char -> Bool
isNewLine x = x `elem` ['\n', '\r']

