{-# LANGUAGE OverloadedStrings #-}
module RawForwarder where

import Network.Wai.Middleware.RequestLogger (logStdout)

import qualified Data.ByteString as DB
import qualified Data.ByteString.Builder as DBB
import qualified Network.HTTP.Client as NHC
import qualified Network.Wai as NW
import qualified Network.Wai.Handler.Warp as NWHW

rawForwarder :: String -> IO ()
rawForwarder rawUpstreamAddress = do
    -- Make a request and a connection manager.
    request <- NHC.parseRequest rawUpstreamAddress
    manager <- NHC.newManager NHC.defaultManagerSettings
    -- Run a server to handle downstream requests and fulfill them with
    -- upstream requests.
    NWHW.runEnv 80
        . logStdout
        $ forwardAnyRequest request manager

forwardAnyRequest :: NHC.Request -> NHC.Manager -> NW.Application
forwardAnyRequest requestTemplate manager downstreamRequest sendResponse = do
    -- Make a request to send upstream.
    request <- convertRequest requestTemplate downstreamRequest
    -- Execute the request and handle the response.
    NHC.withResponse request manager $ \response -> do
        -- Make a response to send back downstream.
        convertResponse response >>= sendResponse

-- | To proxy the Wai request upstream, convert it to a HTTP-Client request.
convertRequest :: NHC.Request -> NW.Request -> IO NHC.Request
convertRequest requestTemplate downstreamRequest = do
    body <- NW.lazyRequestBody downstreamRequest
    return $ requestTemplate
        { NHC.method         = NW.requestMethod downstreamRequest
        , NHC.path           = NW.rawPathInfo downstreamRequest -- XXX: Using rawPathInfo might cause bugs? See documentation.
        , NHC.queryString    = NW.rawQueryString downstreamRequest
        , NHC.requestHeaders = NW.requestHeaders downstreamRequest
        , NHC.requestBody    = NHC.RequestBodyLBS body
        }

-- | To proxy the HTTP-Client response downstream, convert it to a Wai
-- response.
convertResponse :: NHC.Response NHC.BodyReader -> IO NW.Response
convertResponse upstreamResponse = do
    body <- fmap DBB.byteString . fmap DB.concat . NHC.brConsume
        $ NHC.responseBody upstreamResponse
    return $ NW.responseBuilder
        (NHC.responseStatus upstreamResponse)
        (NHC.responseHeaders upstreamResponse)
        body
