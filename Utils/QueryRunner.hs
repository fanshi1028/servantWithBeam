{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Utils.QueryRunner where

import Colog (LogAction (unLogAction), Message, logDebug, logTextStdout)
import Control.Natural (type (~>))
import Database.Beam.Postgres (Pg, runBeamPostgresDebug)
import qualified Database.Beam.Postgres as Pg (Connection)
import Database.Beam.Sqlite (SqliteM, runBeamSqliteDebug)
import qualified Database.SQLite.Simple as Lite (Connection)
import Servant (Handler)
import Universum
import UnliftIO.Pool (withResource)
import Utils.Types (MyServer)

doPgQueryWithDebug :: Pg ~> MyServer be db Pg.Connection Message Handler
doPgQueryWithDebug pg = do
  pool <- view #_pool <$> ask
  logDebug "hi"
  liftIO $ withResource pool (flip (runBeamPostgresDebug $ unLogAction logTextStdout . fromString) pg)

doSqliteQueryWithDebug :: (MonadIO m) => (SqliteM ~> ReaderT Lite.Connection m)
doSqliteQueryWithDebug = ReaderT . (liftIO <$>) <$> flip (runBeamSqliteDebug putStrLn)

