{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai
import Network.Wai.Handler.Warp (Port)
import Types

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as T
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Network.HTTP (Response(..), simpleHTTP, getRequest)
import System.Environment (getArgs)

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
