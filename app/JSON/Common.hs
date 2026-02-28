module JSON.Common where

import qualified Data.ByteString.Builder as BB
import Data.List (intersperse)

data Json = JBool Bool
          | JNumber Double
          | JString String
          | JNull
          | JArray [Json]
          | JObject [(String, Json)]
          deriving Show

-- mconcat (intersperse (BB.charUtf8 ',') $ map packJson xs) <> BB.charUtf8 ']'
packJson :: Json -> BB.Builder
packJson (JBool True)   = BB.stringUtf8 "true"
packJson (JBool False)  = BB.stringUtf8 "false"
packJson (JNumber x)    = BB.stringUtf8 $ show x
packJson (JString s)    = BB.charUtf8 '"' <> BB.stringUtf8 s <> BB.charUtf8 '"'
packJson JNull          = BB.stringUtf8 "null"
packJson (JArray xs)    = BB.charUtf8 '[' <> mconcat (intersperse (BB.charUtf8 ',') $ map packJson xs) <> BB.charUtf8 ']'
packJson (JObject obj)  = BB.charUtf8 '{' <> mconcat (intersperse (BB.charUtf8 ',') $ map packEntry obj) <> BB.charUtf8 '}'
                          where
                          packEntry (k, v) 
                            = packJson (JString k) <> BB.stringUtf8 ":" <> packJson v
