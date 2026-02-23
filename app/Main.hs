module Main where

import Data.Char (ord)
import Network.Simple.TCP
import qualified Data.ByteString.Internal as BI
import HTTP.Request
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
                -- Fancy prints it which is bad.
                -- Either just fold the damn thing into string
                -- or better yet; make parser use bytestring <- should be pretty easy to do in <30 mins
                let x' = show x
                let parsed = parse p x'
                print $ map ord x'
                case parsed of
                    Nothing -> send sock (BI.packChars ("Failed to parse: " ++ x')) >> print ("Failed to parse: " ++ x')
                    Just (req, _) -> do
                        let req' = BI.packChars $ show req
                        send sock req' >> print (parse p (show req))
    return()

