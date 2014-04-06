{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Trans (lift)
import Data.Conduit
import Data.Conduit.Network.UDP
import Data.Aeson
import Control.Monad.IO.Class
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runStateT)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Text as T

import Notification
import Store

udpPort           = 5555
maxUdpFrameBytes  = 8192

config = Config{
    dbname = "railoscopy"
  , user = "ababkin"
  , password = "izmail"
  }

main = do
  flip runStateT Nothing $ do
    flip runReaderT config $ do
      migrate
      getUdpMessage $$ extractData =$= parseNotification =$ handleNotification
      where
        getUdpMessage = do
          socket <- liftIO $ bindPort udpPort HostAny
          sourceSocket socket maxUdpFrameBytes 

        extractData :: Conduit Message RSY BS.ByteString
        extractData = do
          awaitForever $ \msg ->
            yield $ msgData msg

        parseNotification :: Conduit BS.ByteString RSY Notification
        parseNotification = do
          CA.conduitParserEither json =$= unwrap
          where
            unwrap = awaitForever $ \case
              (Left e) -> do
                yield $ parseErrorNotification $ T.pack $ show e
                {- parseNotification -}
                
              (Right (_, j)) -> do
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

        {- handleNotification :: (MonadReader Config m, MonadIO m) => Sink Notification m () -}
        handleNotification :: Sink Notification RSY ()
        handleNotification = do
          awaitForever $ \notification -> do
            {- liftIO $ putStr $ show notification -}
            lift $ saveNotification notification




