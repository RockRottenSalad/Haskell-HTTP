module HTTP.Common where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import JSON.Common

data Header = Header
    {    key     :: BS.ByteString
    ,    value   :: BS.ByteString 
    } deriving (Show)

newHeader :: String -> String -> Header
newHeader k v = Header 
    {   key = BI.packChars k
    ,   value = BI.packChars v
    }

packHeader :: Header -> BB.Builder
packHeader (Header key value) = BB.byteString key <> BB.stringUtf8 ": " <> BB.byteString value

type ProtocolVersion = (Int, Int)

packProtocolVersion :: ProtocolVersion -> BB.Builder
packProtocolVersion (a, b) = BB.stringUtf8 "HTTP/" <> BB.stringUtf8 (show a) <> BB.charUtf8 '.' <> BB.stringUtf8 (show b)

data Contents = None
              | Text String 
              | JSON Json
              deriving Show

packContents :: Contents -> BB.Builder
packContents None     = mempty
packContents (Text s) = BB.stringUtf8 s
packContents (JSON j) = packJson j

type Path = [BS.ByteString]
