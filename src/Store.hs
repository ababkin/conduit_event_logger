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
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Aeson (encode)
import Control.Lens
import Data.Functor ((<$>))
import Control.Monad.Reader (asks)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL

import Notification
import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  NotificationRecord
    sourceType  String
    eventType   String
    payload     String
    timestamp   String Maybe
    start       Int Maybe
    duration    Int Maybe
    deriving Show
|]

{- connStr = "host=localhost dbname=event_logger user=gust port=5432" -}
{- connStr = "host=localhost dbname=event_logger user=ababkin password=izmail port=5432" -}

getConnectionString :: RSY BS.ByteString
getConnectionString = do
  db <- asks dbname
  u <- asks user
  p <- asks password
  return $ BS.pack $ "host=localhost port=5432 dbname=" ++ db ++ " user=" ++ u ++ " password=" ++ p

migrate :: RSY ()
migrate = do
  connStr <- getConnectionString
  liftIO $ withPostgresqlPool connStr 10 $ \pool -> do
    flip runSqlPersistMPool pool $ do
      runMigration migrateAll
  return ()


saveNotification :: Notification -> RSY ()
saveNotification n = do
  connStr <- getConnectionString
  liftIO $ withPostgresqlPool connStr 10 $ \pool -> do
    flip runSqlPersistMPool pool $ do
      let notificationRecord = toNotificationRecord n
      {- liftIO $ putStrLn $ "inserting: " ++ (show notificationRecord) -}
      notificationId <- insert notificationRecord
      return ()


toNotificationRecord :: Notification -> NotificationRecord
toNotificationRecord n = NotificationRecord
                          (T.unpack $ n^.sourceType)
                          (show $ n^.eventType)
                          (BL.unpack $ encode $ n^.payload)
                          (T.unpack <$> (n^.timestamp))
                          (n^.start)
                          (n^.duration)

