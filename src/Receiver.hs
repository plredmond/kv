module Receiver where

import Control.Monad.Trans (liftIO)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant ((:<|>)(..))

import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map.Strict as Map
import qualified Network.Wai.Handler.Warp as NWHW
import qualified Servant as S

import qualified API

receiver :: IO ()
receiver = do
    -- Note: In this quick example code we're not using this MVar correctly.
    -- This example doesn't evaluate & force work done on the contents of the
    -- MVar, which might cause work to bleed into the wrong thread.
    state <- MVar.newMVar Map.empty
    NWHW.runEnv 80
        . logStdout
        . S.serve API.proxy
        . receivingEndpoints
        $ state

-- | Define endpoints that handle state.
receivingEndpoints :: MVar.MVar (Map.Map String Int) -> S.Server API.KeyValAPI
receivingEndpoints state = handleGet :<|> handlePut :<|> handleDelete
  where
    -- Retrieve the value associated with a key.
    handleGet key = do
        -- Warning: readMVar may block indefinitely
        m <- liftIO $ MVar.readMVar state
        return . API.GetResponse $ Map.lookup key m
    -- Replace the value associated with a key and return the old value.
    handlePut key (API.PutData val) = do
        -- Warning: modifyMVar may block indefinitely and may not be atomic
        old <- liftIO $ MVar.modifyMVar state $ \m -> do
            return (Map.insert key val m, Map.lookup key m)
        return $ API.PutResponse old
    -- Remove the association with the key and return the old value.
    handleDelete key = do
        -- Warning: modifyMVar may block indefinitely and may not be atomic
        old <- liftIO $ MVar.modifyMVar state $ \m -> do
            return (Map.delete key m, Map.lookup key m)
        return $ API.DeleteResponse old

