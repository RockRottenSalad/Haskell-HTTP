module Main where

--import Network.Simple.TCP
--import qualified Data.ByteString.Internal as BI
--import qualified Data.ByteString as BS
--import Parser
import HTTP.Request
import HTTP.Response
import HTTP.Common
import JSON.Common
import API.Common

appConfig :: App
appConfig = App 
    {   port = 8080
    ,   endpoints = [
         Endpoint { uri = "/",     method = GET,  handle = hello } 
       , Endpoint { uri = "/echo", method = POST, handle = echoContents } 
       , Endpoint { uri = "/echo", method = GET,  handle = const $ textResponse "Expected POST for this endpoint" } 
       , Endpoint { uri = "/query", method = GET, handle = echoQueryParameters } 
       ]
    }

hello :: Request -> Response
hello _ = jsonResponse $ JObject [
                           ("Message", JString "Hello, World!"),
                           ("numbersArray", JArray [JNumber 4, JNumber 3.14])
                         ]

echoQueryParameters :: Request -> Response
echoQueryParameters req = textResponse $ "Got query parameters: " ++ show (parameters req)

echoContents :: Request -> Response
echoContents req = case contents req of
                        None -> defaultResponse {
                                  headers = [ newHeader "content-type" "text/plain" ]
                                , replyContents = Text "Got nothing"
                                }
                        Text t -> defaultResponse {
                                  headers = [ newHeader "content-type" "text/plain" ]
                                , replyContents = Text t
                                }
                        JSON j -> defaultResponse {
                                  headers = [ newHeader "content-type" "application/json" ]
                                , replyContents = JSON j
                                }
main :: IO()
main = runApp appConfig

--main :: IO()
--main = do
--    let requestParser = parseRequest
--    _ <- serve (Host "127.0.0.1") "8080" $ \(sock, remoteAddr) -> do
--        putStrLn $ "TCP connected from: " ++ show remoteAddr
--        input <- recv sock 1024
--        case input of
--            Nothing -> putStrLn "Couldn't receive input"
--            Just x -> do
--                let stringifiedInput = map BI.w2c $ BS.unpack x
--                print stringifiedInput
--                let parsed = parse requestParser stringifiedInput
--                case parsed of
--                    Nothing -> send sock (BI.packChars ("Failed to parse: " ++ stringifiedInput)) >> print ("Failed to parse: " ++ stringifiedInput)
--                    Just (req, _) -> do
--                        let response = echoContents req 
--                        let responsePacked = packResponse response
--                        print $ show req
--                        send sock responsePacked
--    return ()
--
