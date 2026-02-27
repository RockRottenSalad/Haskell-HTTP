module HTTP.Response where

import HTTP.Common
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BB

import Data.List (intersperse)


data StatusCode = OK | BadRequest | NotFound deriving Show

getStatusCodeValue :: StatusCode -> Int
getStatusCodeValue OK         = 200
getStatusCodeValue BadRequest = 400
getStatusCodeValue NotFound   = 404

packStatusCode :: StatusCode -> BB.Builder
packStatusCode = BB.stringUtf8 . show 

data Response = Response
    {   version :: ProtocolVersion 
    ,   status  :: StatusCode   
    ,   headers :: [Header]
    ,   contents :: String
    } deriving Show

packResponse :: Response -> BS.ByteString
packResponse r = BSL.toStrict $ BB.toLazyByteString $ packedVersion <> space <> packedStatusCode <> space <> statusMessage <> newline <> packedHeaders <> newline <> newline <> BB.stringUtf8 (contents r)
    where

    space = BB.charUtf8 ' '
    newline = BB.stringUtf8 "\r\n"
    packedVersion = packProtocolVersion $ version r

    statusCode = status r
    packedStatusCode = BB.stringUtf8 $ show $ getStatusCodeValue statusCode
    statusMessage = packStatusCode statusCode

    packedHeaders = mconcat $ intersperse newline $ map packHeader (headers r)

defaultResponse :: Response
defaultResponse = Response
    {   version = (1,1)
    ,   status = OK
    ,   headers = [ newHeader "content-type" "text/html" ]
    ,   contents = "HELLO, WORLD!"
    }

setContents :: String -> Response -> Response
setContents s r = r { contents = s }

