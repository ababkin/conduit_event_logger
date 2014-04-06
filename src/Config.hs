module Config where

import Control.Monad.Reader (ReaderT)

type RSY a = ReaderT Config IO a
{- type RSY a = ConfiguredT IO a -}
{- type ConfiguredT m a = ReaderT Config m a -}

data Config = Config {
    dbname   :: String
  , user     :: String
  , password :: String
  }
