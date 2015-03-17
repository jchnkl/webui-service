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

    {-
    get "/anagram" $ do
        -- p <- fst . head <$> params
        -- params >>= liftIO . print
        -- liftIO $ print $ anagramURL p
        -- httpRequest (anagramURL p) >>= \result -> case result of
        --     Left cerr -> json . Error 500 "Internal Server Error" . Just . T.pack $ show cerr
        --     Right rsp -> text . T.pack $ rspBody rsp

        BS.unpack . rawQueryString <$> request >>= httpRequest . anagramURL >>= sendReply

    get "/complete" $ do
        -- queryString <$> request >>= liftIO . print
        -- pathInfo <$> request >>= liftIO . print
        -- liftIO $ print "complete"
        -- res <- redirect "http://127.0.0.1:1880"
        -- liftIO $ print res
        -- text res

        BS.unpack . rawQueryString <$> request
            >>= httpRequest . completeURL >>= sendReply

        -- redirect "http://127.0.0.1:1880" >>= text
        -- p <- fst . head <$> params
        -- httpRequest (completeURL p) >>= \result -> case result of
        --     Left cerr -> json . Error 500 "Internal Server Error" . Just . pack $ show cerr
        --     Right rsp -> text . pack $ rspBody rsp
    -}

    -- where
    -- httpRequest = liftIO . simpleHTTP . getRequest
    -- sendReply er = case er of
    --     Left cerr -> json . Error 500 "Internal Server Error" . Just . T.pack $ show cerr
    --     Right rsp -> text . T.pack $ rspBody rsp
    -- anagramURL  r = "http://127.0.0.1:1525/anagram" ++ r
    -- completeURL r = "http://127.0.0.1:1880/complete" ++ r
