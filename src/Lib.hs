{-# LANGUAGE LambdaCase #-}
module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import Receiver (receiver)
import Forwarder (forwarder)

someFunc :: IO ()
someFunc = do
    getArgs >>= \case
        [address] -> do
            putStrLn "starting a forwarder"
            forwarder address
        [] -> do
            putStrLn "starting a receiver"
            receiver
        _ -> do
            putStrLn "USAGE:"
            putStrLn "  receiver LISTEN-PORT"
            putStrLn "  forwarder LISTEN-PORT RECEIVER-ADDR"
