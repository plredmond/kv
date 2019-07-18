{-# LANGUAGE OverloadedStrings #-}
module RawForwarder where

import Network.Wai.Middleware.RequestLogger (logStdout)

import qualified Data.ByteString as DB
import qualified Data.ByteString.Builder as DBB
import qualified Network.HTTP.Client as Client
import qualified Network.Wai as Server
import qualified Network.Wai.Handler.Warp as NWHW

rawForwarder :: String -> IO ()
rawForwarder rawUpstreamAddress = do
    -- Make a request template and a connection manager. We'll reuse the
    -- scheme, host, and port information in the request template, while
    -- overwriting the other properties.
    requestTemplate <- Client.parseRequest rawUpstreamAddress
    manager <- Client.newManager Client.defaultManagerSettings
    -- Run a server to handle downstream requests and fulfill them with
    -- upstream requests.
    NWHW.runEnv 80
        . logStdout
        $ forwardAnyRequest requestTemplate manager

forwardAnyRequest :: Client.Request -> Client.Manager -> Server.Application
forwardAnyRequest requestTemplate manager downstreamRequest sendResponse = do
    -- Make a request to send upstream.
    request <- convertRequest requestTemplate downstreamRequest
    -- Execute the request and handle the response.
    Client.withResponse request manager $ \response -> do
        -- Make a response to send back downstream.
        convertResponse response >>= sendResponse

-- | To proxy the Wai request upstream, convert it to a HTTP-Client request.
convertRequest :: Client.Request -> Server.Request -> IO Client.Request
convertRequest requestTemplate downstreamRequest = do
    body <- Server.lazyRequestBody downstreamRequest
    return $ requestTemplate
        { Client.method         = Server.requestMethod downstreamRequest
        , Client.path           = Server.rawPathInfo downstreamRequest -- XXX: Using rawPathInfo might cause bugs? See documentation.
        , Client.queryString    = Server.rawQueryString downstreamRequest
        , Client.requestHeaders = Server.requestHeaders downstreamRequest
        , Client.requestBody    = Client.RequestBodyLBS body
        }

-- | To proxy the HTTP-Client response downstream, convert it to a Wai
-- response.
convertResponse :: Client.Response Client.BodyReader -> IO Server.Response
convertResponse upstreamResponse = do
    body <- fmap DBB.byteString . fmap DB.concat . Client.brConsume
        $ Client.responseBody upstreamResponse
    return $ Server.responseBuilder
        (Client.responseStatus upstreamResponse)
        (Client.responseHeaders upstreamResponse)
        body
