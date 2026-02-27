module HTTP.Common where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI

data Header = Header
    {    getKey     :: BS.ByteString
    ,    getValue   :: BS.ByteString 
    } deriving (Show)

newHeader :: String -> String -> Header
newHeader k v = Header 
    {   getKey = BI.packChars k
    ,   getValue = BI.packChars v
    }

packHeader :: Header -> BB.Builder
packHeader x = BB.byteString (getKey x) <> BB.stringUtf8 ": " <> BB.byteString (getValue x)

type ProtocolVersion = (Int, Int)

packProtocolVersion :: ProtocolVersion -> BB.Builder
packProtocolVersion (a, b) = BB.stringUtf8 "HTTP/" <> BB.stringUtf8 (show a) <> BB.charUtf8 '.' <> BB.stringUtf8 (show b)

type Path = [BS.ByteString]
