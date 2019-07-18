{-# LANGUAGE LambdaCase #-}
module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import Receiver (receiver)
import Forwarder (forwarder)
import Control.Exception (bracket_)

someFunc :: IO ()
someFunc = do
    getArgs >>= \case
        [address] -> bracket_
            (putStrLn $ "starting a forwarder, " ++ address)
            (putStrLn $ "forwarder stopping, " ++ address)
            (forwarder address)
        [] -> bracket_
            (putStrLn "starting a receiver")
            (putStrLn "receiver stopping")
            receiver
        _ -> do
            putStrLn "USAGE:"
            putStrLn "  env PORT=<PORT> kv-server # start a receiver"
            putStrLn "  env PORT=<PORT> kv-server <RECEIVER-ADDR> # start a forwarder"
