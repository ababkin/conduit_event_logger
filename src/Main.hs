{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans
import Data.Conduit
import Data.Conduit.Network.UDP
import Data.Aeson
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.Attoparsec as CA

import Notification

source = do
  socket <- liftIO $ bindPort 5555 HostAny
  sourceSocket socket 1024

extractData :: Conduit Message IO BS.ByteString
extractData = do
  maybeMessage <- await
  case maybeMessage of
    Nothing -> liftIO $ putStrLn "No data left to extract, exiting"
    Just msg -> do
      yield $ msgData msg
      extractData

parseNotification :: Conduit BS.ByteString IO Notification
parseNotification = do
  CA.conduitParserEither json =$= awaitForever go
  where
    go (Left s) = error $ show s
    go (Right (_, j)) = do
      case fromJSON j :: Result Notification of
        Success notification  -> yield notification
        Error s               -> error s

sink = do
  liftIO $ putStrLn $ "sink: waiting for notifications..."
  maybeNotification <- await
  case maybeNotification of
    Nothing -> liftIO $ putStrLn "sink: Nothing from upstream, exiting"
    Just notification -> do
      liftIO $ putStr $ show notification
      sink
            
main = source $$ extractData =$= parseNotification =$ sink
  



