module JSON.Common where

data Json = JBool Bool
          | JNumber Double
          | JString String
          | JArray [Json]
          | JObject [(String, Json)]
          | JNull deriving Show

