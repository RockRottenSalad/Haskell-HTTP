module API.Common where

import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString as BS
import HTTP.Common
import HTTP.Request
import HTTP.Response
import Network.Simple.TCP
import Parser
import Data.List (find)

data Endpoint = Endpoint
    {   method   :: Method
    ,   uri      :: String
    ,   handle   :: Request -> Response
    }

data App = App
    {   endpoints :: [Endpoint]
    ,   port :: Int
    }

requestMathesEndpoint :: Request -> Endpoint -> Bool
requestMathesEndpoint req end = extractPath req == uri end && getMethod req == method end

-- Just placeholder garbage to get stuff working
runApp :: App -> IO()
runApp app = do
    serve (Host "127.0.0.1") (show $ port app) $ \(sock, remoteAddr) -> do
        putStrLn $ "TCP connected from: " ++ show remoteAddr
        input <- recv sock 1024
        case input of
            Nothing -> putStrLn "Couldn't receive input"
            Just x -> do
                let stringifiedInput = map BI.w2c $ BS.unpack x
                print stringifiedInput
                let parsed = parse parseRequest stringifiedInput
                case parsed of
                    Nothing -> send sock (BI.packChars ("Failed to parse: " ++ stringifiedInput)) >> print ("Failed to parse: " ++ stringifiedInput)
                    Just (req, _) -> do
                        case find (requestMathesEndpoint req) (endpoints app) of
                            Nothing -> send sock $ packResponse (textResponse "Unknown endpoint")
                            Just (Endpoint _ _ handle') -> send sock $ packResponse $ handle' req
        return ()


