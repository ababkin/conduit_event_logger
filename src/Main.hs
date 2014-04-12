{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Trans    (lift)
import Control.Exception.Extensible as E
import Network.BSD            (getProtocolNumber)
import Network.Socket         ( Socket, SockAddr(..), SocketOption(..)
                              , SocketType(Stream), SocketType(Datagram)
                              , Family(AF_INET) , accept, bindSocket
                              , iNADDR_ANY, sClose, listen, maxListenQueue
                              , setSocketOption, socket
                              )
import Data.Conduit
import Data.Conduit.Network.UDP
import Data.Aeson
import Control.Monad.IO.Class
import Control.Monad.Reader   (runReaderT)
import Control.Monad.State    (runStateT)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Text as T

import Notification
import Store

udpPort           = 5555
maxUdpFrameBytes  = 8192

config = Config{
    dbname    = "railoscopy"
  , user      = "ababkin"
  , password  = "izmail"
  }

main = do
  flip runStateT Nothing $ do
    flip runReaderT config $ do
      migrate
      getUdpMessage $$ extractData =$= parseNotification =$ handleNotification
      where
        getUdpMessage = do
          socket <- liftIO $ listenUDP udpPort
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
                yield $ parseErrorNotification $ show e
                
              (Right (_, j)) -> do
                yield $ case fromJSON j :: Result Notification of
                  Success notification  -> notification
                  Error s               -> parseErrorNotification s

            parseErrorNotification s = Notification{
              _sourceType = "internal"
            , _eventType  = ParseError
            , _payload    = ParseErrorPayload s
            , _timestamp  = ""
            , _start      = 0
            , _duration   = 0
            }

        handleNotification :: Sink Notification RSY ()
        handleNotification = do
          awaitForever $ \notification -> do
            lift $ saveNotification notification



-- taken from https://github.com/joehillen/acme-sip/blob/master/Acme/Serve.hs
listenUDP :: Int  -- ^ port number
         -> IO Socket
listenUDP portm = do
    proto <- getProtocolNumber "udp"
    E.bracketOnError
        (socket AF_INET Datagram proto)
        sClose
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (SockAddrInet (fromIntegral portm) iNADDR_ANY)
            return sock
        )
