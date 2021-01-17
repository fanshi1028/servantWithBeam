{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Utils.QueryRunner where

import Colog (LogAction (unLogAction), Message, WithLog, logDebug, logMsg, logTextStdout)
import Control.Natural (type (~>))
import Database.Beam (HasQBuilder, MonadBeam)
import Database.Beam.Postgres (Pg, Postgres, runBeamPostgresDebug)
import qualified Database.Beam.Postgres as Pg (Connection)
import Database.Beam.Sqlite (SqliteM, runBeamSqliteDebug)
import qualified Database.SQLite.Simple as Lite (Connection)
import Servant (Handler, NoContent (NoContent))
import Universum
import UnliftIO (MonadUnliftIO (withRunInIO))
import UnliftIO.Pool (Pool, withResource)
import Utils.Types (MyServer)

doPgQueryWithDebug :: Pg ~> MyServer be db Pg.Connection Message Handler
doPgQueryWithDebug pg = do
  pool <- view #_pool <$> ask
  logDebug "hi"
  liftIO $ withResource pool (flip (runBeamPostgresDebug $ unLogAction logTextStdout . fromString) pg)

-- doPgQueryWithDebug :: (MonadIO m) => (Pg a -> ReaderT Connection m a)
-- doPgQueryWithDebug = ReaderT <$> (liftIO <<$>> flip (runBeamPostgresDebug putStrLn))

-- doPgQueryWithDebug :: Pg ~> MyServer be db conn Text Handler
-- doPgQueryWithDebug = doPgQueryWithDebug' (view #_pool)

doSqliteQueryWithDebug :: (MonadIO m) => (SqliteM ~> ReaderT Lite.Connection m)
doSqliteQueryWithDebug = ReaderT . (liftIO <$>) <$> flip (runBeamSqliteDebug putStrLn)

-- doSqliteQueryWithDebug conn = liftIO <$> runBeamSqliteDebug putStrLn conn

-- instance (MonadIO n) => QueryRunner Pg (ReaderT Pg.Connection n) where
--   doQuery = doPgQueryWithDebug
