{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module Utils.QueryRunner where

import Database.Beam (MonadBeam)
import Universum
import qualified Database.Beam.Postgres as Pg (Connection)
import Database.Beam.Postgres (Postgres, runBeamPostgresDebug, Pg)
import Database.Beam.Sqlite (runBeamSqliteDebug, SqliteM)
import qualified Database.SQLite.Simple as Lite (Connection)

doPgQueryWithDebug' :: (MonadIO m) => (env -> Pg.Connection) -> (Pg a -> ReaderT env m a)
doPgQueryWithDebug' extractFromEnv = ReaderT <$> (liftIO <<$>> flip (runBeamPostgresDebug putStrLn . extractFromEnv))

-- doPgQueryWithDebug :: (MonadIO m) => (Pg a -> ReaderT Connection m a)
-- doPgQueryWithDebug = ReaderT <$> (liftIO <<$>> flip (runBeamPostgresDebug putStrLn))

doPgQueryWithDebug :: (MonadIO m) => (Pg a -> ReaderT Pg.Connection m a)
doPgQueryWithDebug = doPgQueryWithDebug' id

doSqliteQueryWithDebug :: (MonadIO m) => (SqliteM a -> ReaderT Lite.Connection m a)
doSqliteQueryWithDebug = ReaderT . (liftIO <$>) <$> flip (runBeamSqliteDebug putStrLn)

-- doSqliteQueryWithDebug conn = liftIO <$> runBeamSqliteDebug putStrLn conn

class QueryRunner m n where
  doQuery :: (MonadBeam be m, Monad n) => forall t. m t -> n t

instance (MonadIO n) => QueryRunner Pg (ReaderT Pg.Connection n) where
  doQuery = doPgQueryWithDebug
