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

import Colog (HasLog (..), LogAction, LoggerT (..))
import Control.Monad.Except (MonadError)
import Data.Attoparsec.Text (Parser, char, parseOnly, string, takeText, takeTill)
import Data.Generics.Labels ()
import Database.Beam (DatabaseEntity, DatabaseSettings, TableEntity)
import Database.Beam.Postgres (ConnectInfo (..))
import Servant (ServerError)
import Servant.Auth.Server (CookieSettings, JWTSettings)
import System.Envy (FromEnv (..), Var (..), env, envMaybe, (.!=))
import System.Metrics.Counter (Counter)
import Universum
import UnliftIO (MonadUnliftIO)
import UnliftIO.Pool (Pool)

-- | ConnectInfo
-- >>> parseOnly parseDatabaseUrl "postgres://user:pw@host:port/db"
-- Right (ConnectInfo {connectHost = "host", connectPort = 5432, connectUser = "user", connectPassword = "pw", connectDatabase = "db"})
parseDatabaseUrl :: Parser ConnectInfo
parseDatabaseUrl =
  string "postgres://" *> do
    let parseTill c trans = trans <$> takeTill (== c) <* char c
    user <- parseTill ':' toString
    pw <- parseTill '@' toString
    host <- parseTill ':' toString
    port <- parseTill '/' $ fromRight 5432 . readEither
    db <- toString <$> takeText
    return $ ConnectInfo host port user pw db

instance Var ConnectInfo where
  fromVar = rightToMaybe . parseOnly parseDatabaseUrl . fromString

instance FromEnv ConnectInfo where
  fromEnv _ =
    env "DATABASE_URL"
      <|> ConnectInfo
        <$> envMaybe "PG_HOST" .!= "localhost"
          <*> envMaybe "PG_PORT" .!= 5432
          <*> env "PG_USER"
          <*> envMaybe "PG_PW" .!= ""
          <*> env "HITMEN_DB"

-- | LoggerT
deriving newtype instance (MonadUnliftIO m) => MonadUnliftIO (LoggerT msg m)

-- | Env
data Env be db conn logger = Env
  { _logger :: logger,
    _cs :: CookieSettings,
    _jwts :: JWTSettings,
    _state :: TVar Int,
    _counter :: Counter,
    _db :: DatabaseSettings be db,
    _pool :: Pool conn
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
