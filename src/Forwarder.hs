module Forwarder where

import Control.Monad ((<=<))
import Control.Monad.Trans (liftIO)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant ((:<|>)(..))

import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified Network.HTTP.Client as NHC
import qualified Network.Wai.Handler.Warp as NWHW
import qualified Servant as S
import qualified Servant.Client as SC

import qualified API

forwarder :: String -> IO ()
forwarder upstreamRaw = do
    -- Parse the upstream url and set up a connection manager.
    upstream <- SC.parseBaseUrl upstreamRaw
    manager <- NHC.newManager NHC.defaultManagerSettings
    -- Run a server to handle downstream requests and fulfill them with
    -- upstream requests.
    NWHW.runEnv 80
        . logStdout
        . S.serve API.proxy
        . forwardingEndpoints
        $ SC.mkClientEnv manager upstream

-- | Define endpoints that forward each of API.KeyValAPI's requests.
forwardingEndpoints :: SC.ClientEnv -> S.Server API.KeyValAPI
forwardingEndpoints clientEnv = handleGet :<|> handlePut :<|> handleDelete
  where
    -- Extract API.KeyValAPI client functions for making upstream requests. The
    -- extracted functions are already transformed to the servant server
    -- Handler monad.
    callGet :<|> callPut :<|> callDelete
        = SC.hoistClient API.proxy transformMonad
        $ SC.client API.proxy
    -- Transform a Client monad to a Handler monad.
    transformMonad :: SC.ClientM a -> S.Handler a
    transformMonad
        = either (S.throwError . translateError) return
        <=< liftIO
        . flip SC.runClientM clientEnv
    -- Define the endpoints.
    handleGet = callGet
    handlePut = callPut
    handleDelete = callDelete

-- | Translate upstream errors into downstream errors so that the forwarding
-- endpoints expose any failures that occur.
--
-- This is an exceedingly lazy implementation that just stuffs the client error
-- into the body of a 500.
translateError :: SC.ServantError -> S.ServantErr
translateError err = S.err500 { S.errBody = DBLC.pack $ show err}
