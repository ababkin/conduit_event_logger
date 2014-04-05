{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans
import Data.Conduit
import Data.Conduit.Network.UDP
import qualified Data.ByteString.Char8 as BS


source = do
  socket <- liftIO $ bindPort 5555 HostAny
  sourceSocket socket 1024
    
conduit = do
  liftIO $ putStrLn "conduit calling await"
  mx <- await
  case mx of
    Nothing -> liftIO $ putStrLn "Nothing left, exiting"
    Just x -> do
      liftIO $ putStrLn $ "conduit yielding"
      yield x
      conduit
            
sink = do
  liftIO $ putStrLn $ "sink: waiting for messages..."
  maybeMsg <- await
  case maybeMsg of
    Nothing -> liftIO $ putStrLn "sink: Nothing from upstream, exiting"
    Just msg -> do
      liftIO $ putStr $ BS.unpack $ msgData msg
      sink
            
main = source $$ conduit =$ sink
  



