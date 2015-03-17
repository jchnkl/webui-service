{-# LANGUAGE OverloadedStrings #-}

import Network.Wai.Handler.Warp (Port)
import System.Environment (getArgs)
import Web.Scotty
import Types

defaultPort :: Port
defaultPort = 8888

runScotty :: Port -> IO ()
runScotty port = scotty port $ do
    get "/"  $ file "index.html"
    notFound $ json $ Error 400 "service not found" Nothing

main :: IO ()
main = do
    args <- getArgs
    case length args of
        1 -> runScotty $ readPort args
        _ -> runScotty defaultPort
    where readPort args = read (head args) :: Port
