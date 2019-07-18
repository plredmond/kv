{-# LANGUAGE LambdaCase #-}
module Lib
    ( someFunc
    ) where

import Control.Exception (bracket_)
import Forwarder (forwarder)
import RawForwarder (rawForwarder)
import Receiver (receiver)
import System.Environment (getArgs)

someFunc :: IO ()
someFunc = do
    getArgs >>= \case
        ["raw", address] -> bracket_
            (putStrLn $ "starting a raw-forwarder, " ++ address)
            (putStrLn $ "raw-forwarder stopping, " ++ address)
            (rawForwarder address)
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
