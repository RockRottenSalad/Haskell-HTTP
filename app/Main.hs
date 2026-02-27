module Main where

import Data.Char (ord)
import Network.Simple.TCP
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString as BS
import HTTP.Request
import HTTP.Response
import Parser

main :: IO()
main = do
    let p = parseRequest
    _ <- serve (Host "127.0.0.1") "8080" $ \(sock, remoteAddr) -> do
        putStrLn $ "TCP connected from: " ++ show remoteAddr
        input <- recv sock 1024
        case input of
            Nothing -> putStrLn "Couldn't receive input"
            Just x -> do
                let x' = map BI.w2c $ BS.unpack x
                let parsed = parse p x'
                print $ map ord x'
                print x'
                case parsed of
                    Nothing -> send sock (BI.packChars ("Failed to parse: " ++ x')) >> print ("Failed to parse: " ++ x')
                    Just (req, _) -> do
                        let response = setContents ("Your HTTP request: <br>" ++ show req) defaultResponse 
                        let responsePacked = packResponse response
                        print $ show req
                        print responsePacked
                        send sock responsePacked
    return ()

