{-# LANGUAGE OverloadedStrings #-}

import Network.Wai.Handler.Warp (Port)
import System.Environment (getArgs)
import Web.Scotty
import Types

defaultPort :: Port
defaultPort = 8888

runService :: Port -> IO ()
runService port = scotty port $ do
    get "/"  $ file "index.html"
    notFound $ json $ Error 400 "service not found" Nothing

main :: IO ()
main = do
    args <- getArgs
    case length args of
        1 -> runService $ readPort args
        _ -> runService defaultPort
    where readPort args = read (head args) :: Port
