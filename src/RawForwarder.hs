{-# LANGUAGE OverloadedStrings #-}
module RawForwarder where

import Network.Wai.Middleware.RequestLogger (logStdout)

import qualified Data.ByteString as DB
import qualified Data.ByteString.Builder as DBB
import qualified Network.HTTP.Client as NHC
import qualified Network.Wai as NW
import qualified Network.Wai.Handler.Warp as NWHW

rawForwarder :: String -> IO ()
rawForwarder upstreamRaw = do
    -- Make a request and a connection manager.
    request <- NHC.parseRequest upstreamRaw
    manager <- NHC.newManager NHC.defaultManagerSettings
    -- Run a server to handle downstream requests and fulfill them with
    -- upstream requests.
    NWHW.runEnv 80
        . logStdout
        $ forwardAnyRequest request manager

forwardAnyRequest :: NHC.Request -> NHC.Manager -> NW.Application
forwardAnyRequest genericRequestForUpstream manager requestFromDownstream respondToDownstream= do
    -- Make request-for-upstream have the properties of request-from-downstream.
    bodyHeadingUp <- NW.lazyRequestBody requestFromDownstream
    let requestForUpstream = genericRequestForUpstream
            { NHC.method         = NW.requestMethod requestFromDownstream
            , NHC.path           = NW.rawPathInfo requestFromDownstream -- XXX: Using rawPathInfo might cause bugs? See documentation.
            , NHC.queryString    = NW.rawQueryString requestFromDownstream
            , NHC.requestHeaders = NW.requestHeaders requestFromDownstream
            , NHC.requestBody    = NHC.RequestBodyLBS bodyHeadingUp
            }
    -- Execute request-for-upstream.
    NHC.withResponse requestForUpstream manager $ \responseFromUpstream -> do
        -- Extract response-from-upstream body.
        bodyHeadingDown <- fmap DBB.byteString . fmap DB.concat . NHC.brConsume
            $ NHC.responseBody responseFromUpstream
        -- Build and return response-to-downstream.
        respondToDownstream $
            NW.responseBuilder
                (NHC.responseStatus responseFromUpstream)
                (NHC.responseHeaders responseFromUpstream)
                bodyHeadingDown
