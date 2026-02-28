module Main where

import Network.Simple.TCP
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString as BS
import HTTP.Request
import HTTP.Response
import HTTP.Common
import Parser

echoContents :: Request -> Response
echoContents req = case contents req of
                        None -> if null (parameters req)
                                then defaultResponse {
                                  headers = [ newHeader "content-type" "text/plain" ]
                                , replyContents = Text "Got nothing"
                                } else defaultResponse {
                                    headers = [ newHeader "content-type" "text/plain" ]
                                ,   replyContents = Text $ "Got query parameters: " ++ show (parameters req)
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
main = do
    let requestParser = parseRequest
    _ <- serve (Host "127.0.0.1") "8080" $ \(sock, remoteAddr) -> do
        putStrLn $ "TCP connected from: " ++ show remoteAddr
        input <- recv sock 1024
        case input of
            Nothing -> putStrLn "Couldn't receive input"
            Just x -> do
                let stringifiedInput = map BI.w2c $ BS.unpack x
                print stringifiedInput
                let parsed = parse requestParser stringifiedInput
                case parsed of
                    Nothing -> send sock (BI.packChars ("Failed to parse: " ++ stringifiedInput)) >> print ("Failed to parse: " ++ stringifiedInput)
                    Just (req, _) -> do
                        let response = echoContents req 
                        let responsePacked = packResponse response
                        print $ show req
                        send sock responsePacked
    return ()

