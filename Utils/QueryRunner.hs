{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Utils.QueryRunner where

import Data.Pool (Pool, withResource)
import Database.Beam (HasQBuilder, MonadBeam)
import Database.Beam.Postgres (Pg, Postgres, runBeamPostgresDebug)
import qualified Database.Beam.Postgres as Pg (Connection)
import Database.Beam.Sqlite (SqliteM, runBeamSqliteDebug)
import qualified Database.SQLite.Simple as Lite (Connection)
import Servant (NoContent (NoContent))
import Universum
import Control.Natural (type (~>))

doPgQueryWithDebug' :: (MonadIO m) => (env -> Pool Pg.Connection) -> (Pg ~> ReaderT env m)
doPgQueryWithDebug' getPool pg = ReaderT $ \env -> liftIO $ withResource (getPool env) runWithConn
  where
    runWithConn = liftIO . flip (runBeamPostgresDebug putStrLn) pg

-- doPgQueryWithDebug :: (MonadIO m) => (Pg a -> ReaderT Connection m a)
-- doPgQueryWithDebug = ReaderT <$> (liftIO <<$>> flip (runBeamPostgresDebug putStrLn))

doPgQueryWithDebug :: (MonadIO m) => (Pg ~> ReaderT (Pool Pg.Connection) m)
doPgQueryWithDebug = doPgQueryWithDebug' id

doSqliteQueryWithDebug :: (MonadIO m) => (SqliteM ~> ReaderT Lite.Connection m)
doSqliteQueryWithDebug = ReaderT . (liftIO <$>) <$> flip (runBeamSqliteDebug putStrLn)

-- doSqliteQueryWithDebug conn = liftIO <$> runBeamSqliteDebug putStrLn conn

-- class QueryRunner m n where
--   doQuery :: (MonadBeam be m, Monad n) => forall t. m t -> n t

-- instance (MonadIO n) => QueryRunner Pg (ReaderT Pg.Connection n) where
--   doQuery = doPgQueryWithDebug
