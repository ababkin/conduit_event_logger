{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans
import Data.Conduit
import Data.Conduit.Network.UDP
import Data.Aeson
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Text as T

import Notification

udpPort           = 5555
maxUdpFrameBytes  = 8192

main = getUdpMessage $$ extractData =$= parseNotification =$ saveNotification
  where
  getUdpMessage = do
    socket <- liftIO $ bindPort udpPort HostAny
    sourceSocket socket maxUdpFrameBytes 

  extractData :: Conduit Message IO BS.ByteString
  extractData = do
    awaitForever $ \msg ->
      yield $ msgData msg

  parseNotification :: Conduit BS.ByteString IO Notification
  parseNotification = do
    CA.conduitParserEither json =$= awaitForever go
    where
      go (Left e) = do
        yield $ parseErrorNotification $ T.pack $ show e
        {- parseNotification -}
        
      go (Right (_, j)) = do
        {- liftIO $ putStrLn $ "\ngot json chunk of size: " ++ (show $ length $ show j) -}
        yield $ case fromJSON j :: Result Notification of
          Success notification  -> notification
          Error s               -> parseErrorNotification $ T.pack s

      parseErrorNotification s = Notification{
        _sourceType = "internal"
      , _eventType  = ParseError
      , _payload    = ParseErrorPayload s
      , _timestamp  = Nothing
      , _start      = Nothing
      , _duration   = Nothing
      }

  saveNotification = do
    liftIO $ putStrLn $ "sink: waiting for notifications..."
    maybeNotification <- await
    case maybeNotification of
      Nothing -> liftIO $ putStrLn "sink: Nothing from upstream, exiting"
      Just notification -> do
        liftIO $ putStr $ show notification
        saveNotification
            
  



