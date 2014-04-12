{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Store where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Database.Persist hiding (get)
import Database.Persist.Postgresql hiding (get)
import Database.Persist.TH
import Data.Aeson (encode)
import Control.Lens
import Data.Functor ((<$>))
import Control.Monad.Reader (asks)
import Control.Monad.State (get, put)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL

import Notification

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)

data Config = Config {
    dbname   :: String
  , user     :: String
  , password :: String
  }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  NotificationRecord
    parentId    NotificationRecordId Maybe
    sourceType  String
    eventType   String
    payload     String
    timestamp   String
    start       Int
    duration    Int
    deriving Show
|]

type RSY = ReaderT Config (StateT (Maybe NotificationRecordId) IO)

getConnectionString :: RSY BS.ByteString
getConnectionString = do
  db <- asks dbname
  u <- asks user
  p <- asks password
  return $ BS.pack $ "host=localhost port=5432 dbname=" ++ db ++ " user=" ++ u ++ " password=" ++ p

migrate :: RSY ()
migrate = do
  connStr <- getConnectionString
  liftIO $ withPostgresqlPool connStr 100 $ \pool -> do
    flip runSqlPersistMPool pool $ do
      runMigration migrateAll
  return ()


saveNotification :: Notification -> RSY ()
saveNotification notification = do
  connStr <- getConnectionString
  parentNotificationId <- get
  notificationRecordId <- liftIO $ insertNotification connStr parentNotificationId notification
  case notification^.eventType of
    ControllerProcessStart  -> lift $ put $ Just (notificationRecordId :: KeyBackend (PersistEntityBackend NotificationRecord) NotificationRecord)
    ControllerProcessFinish -> lift $ put Nothing
    _                       -> return ()


  where
    insertNotification connStr parentNotificationId notification = do
      withPostgresqlPool connStr 100 $ \pool -> do
        flip runSqlPersistMPool pool $ do
          let notificationRecord = toNotificationRecord parentNotificationId notification
          insert notificationRecord


toNotificationRecord :: Maybe NotificationRecordId -> Notification -> NotificationRecord
toNotificationRecord pid n = NotificationRecord
                          (pid)
                          (T.unpack $ n^.sourceType)
                          (show $ n^.eventType)
                          (BL.unpack $ encode $ n^.payload)
                          (T.unpack $ n^.timestamp)
                          (n^.start)
                          (n^.duration)

