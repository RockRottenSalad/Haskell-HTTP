module HTTP.Response where

import HTTP.Common
import JSON.Common
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BB
import Data.List (intersperse)

data StatusCode = OK | BadRequest | NotFound deriving (Show, Eq)

data Response = Response
    {   version :: ProtocolVersion 
    ,   status  :: StatusCode   
    ,   headers :: [Header]
    ,   replyContents :: Contents
    } deriving Show

getStatusCodeValue :: StatusCode -> Int
getStatusCodeValue OK         = 200
getStatusCodeValue BadRequest = 400
getStatusCodeValue NotFound   = 404

packStatusCode :: StatusCode -> BB.Builder
packStatusCode = BB.stringUtf8 . show 

packResponse :: Response -> BS.ByteString
packResponse r = 
    BSL.toStrict $ BB.toLazyByteString $ 
                    packedVersion <> space <>
                    packedStatusCode <> space <>
                    statusMessage <> newline <>
                    packedHeaders <>
                    newline <> newline <>
                    packContents (replyContents r)
    where

    space = BB.charUtf8 ' '
    newline = BB.stringUtf8 "\r\n"
    packedVersion = packProtocolVersion $ version r

    statusCode = status r
    packedStatusCode = BB.stringUtf8 $ show $ getStatusCodeValue statusCode
    statusMessage = packStatusCode statusCode

    packedHeaders = mconcat $ intersperse newline $ map packHeader (headers r)

-- Response templates
defaultResponse :: Response
defaultResponse = Response
    {   version = (1,1)
    ,   status = OK
    ,   headers = []
    ,   replyContents = None
    }

helloWorldResponse :: Response
helloWorldResponse = defaultResponse
    {   headers = [ newHeader "content-type" "application/json" ]
    ,   replyContents = JSON $ 
            JObject [
                ("Message", JString "Hello, World!"),
                ("Numbers", JArray [JNumber 3.14, JNumber 24])
                ]
    }

jsonResponse :: Json -> Response
jsonResponse json = defaultResponse
    {   headers = [ newHeader "content-type" "application/json" ]
    ,   replyContents = JSON json
    }

textResponse :: String -> Response
textResponse text = defaultResponse
    {   headers = [ newHeader "content-type" "text/plain" ]
    ,   replyContents = Text text
    }

