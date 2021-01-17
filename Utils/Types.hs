{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Types
  ( MyServer (..),
    Env (..),
    TableGetter,
  )
where

import Colog (HasLog (..), LogAction, LoggerT (..), Message)
import Control.Monad.Except (MonadError)
import Control.Natural (type (~>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Generics.Labels ()
import Data.Password.Argon2 (Argon2, PasswordHash (..))
import Database.Beam (DatabaseEntity, DatabaseSettings, FromBackendRow (..), HasSqlEqualityCheck, TableEntity)
import Database.Beam.Postgres (ConnectInfo (..), defaultConnectInfo)
import Servant (Handler (..), ServerError)
import Servant.Auth.Server (CookieSettings, JWTSettings, SetCookie, def)
import Servant.Docs (ToSample (..), singleSample)
import System.Envy (FromEnv (..), env)
import Universum
import UnliftIO (MonadUnliftIO)
import UnliftIO.Pool (Pool)

-- | ConnectInfo
instance FromEnv ConnectInfo where
  fromEnv _ =
    ( \user db ->
        defaultConnectInfo
          & #connectUser .~ user
          & #connectDatabase .~ db
    )
      <$> env "PG_USER"
      <*> env "HITMEN_DB"

-- | LoggerT
deriving newtype instance (MonadUnliftIO m) => MonadUnliftIO (LoggerT msg m)

-- | Env
data Env be db conn logger = Env
  { _logger :: logger,
    _cs :: CookieSettings,
    _jwts :: JWTSettings,
    _pool :: Pool conn,
    _db :: DatabaseSettings be db
  }
  deriving (Generic)

-- | MyServer
newtype MyServer be db conn msg m a = MyServer
  {unMyServer :: ReaderT (Env be db conn $ LogAction (MyServer be db conn msg m) msg) m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (Env be db conn $ LogAction (MyServer be db conn msg m) msg)
    )

instance HasLog (Env be db conn $ LogAction (MyServer be db conn msg m) msg) msg (MyServer be db conn msg m) where
  getLogAction = view #_logger
  setLogAction = set #_logger

deriving newtype instance (MonadError ServerError m) => MonadError ServerError (MyServer be db conn msg m)

-- | TableGetter
type TableGetter be db a = db (DatabaseEntity be db) -> DatabaseEntity be db $ TableEntity a
