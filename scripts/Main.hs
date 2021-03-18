{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Chronos (stopwatch)
import Colog (logInfo)
import Control.Arrow (ArrowChoice ((|||)))
import Database.Beam.AutoMigrate (migrate, printMigration, tryRunMigrationsWithEditUpdate)
import Database.Beam.AutoMigrate.Postgres (getSchema)
import Database.Beam.Postgres (Connection, close, connect, runBeamPostgresDebug)
import Database.PostgreSQL.Simple (withTransaction)
import Migration.Databases.HitmenBusiness (annotatedHitmenBusinessDb, hitmenBusinessDbSchema)
import Options.Generic (ParseRecord, getRecord)
import System.Envy (decodeEnv, env, envMaybe, runEnv, (.!=))
import Universum
import qualified UnliftIO (bracket)
import UnliftIO.Pool (Pool, createPool, destroyAllResources, withResource)

showMigration :: Pool Connection -> IO (Pool Connection)
showMigration conns = do
  withResource conns $ \conn -> do
    withTransaction conn $
      runBeamPostgresDebug putStrLn conn $
        printMigration $ migrate conn hitmenBusinessDbSchema
  return conns

showMigration1 :: Pool Connection -> IO (Pool Connection)
showMigration1 conns = do
  withResource conns $ \conn -> do
    actualSchema <- getSchema conn
    -- print $ diff actualSchema hitmenBusinessDbSchema
    -- putStrLn $ "======================================================"
    -- print $ hitmenBusinessDbSchema
    print actualSchema
  -- print $ diff hitmenBusinessDbSchema actualSchema
  return conns

-- doMigration = tryRunMigrationsWithEditUpdate annotatedHitmenBusinessDb
doMigration :: Pool Connection -> IO (Pool Connection)
doMigration conns = do
  withResource conns $ \conn -> do
    tryRunMigrationsWithEditUpdate annotatedHitmenBusinessDb conn
    putTextLn "Migration done"
    return conns

showAndDoMigration :: Pool Connection -> IO (Pool Connection)
showAndDoMigration conns = do
  void $ showMigration conns
  putTextLn "migrate?"
  getLine >>= \case
    "y" -> doMigration conns
    "Y" -> doMigration conns
    _ -> putTextLn "Exiting" >> return conns

-- NOTE FIXME commandline for running migration script
data Example = Example
  { foo :: Maybe Int,
    bar :: Maybe String,
    yo :: Maybe Text
  }
  deriving (Generic, Show)

instance ParseRecord Example

main :: IO ()
main = do
  x <- getRecord "hi"
  print @Example x
-- NOTE FIXME running migration script
  -- tLog "Get Env: " decodeEnv
  --   >>= logInfo . fromString
  --   ||| flip
  --     withPool
  --     (tLog "do Migration: " . doMigration)
  -- where
  --   tLog context io =
  --     liftIO (stopwatch io)
  --       >>= \(t, a) -> logInfo (context <> show t) >> return a
  --   withPool config =
  --     UnliftIO.bracket
  --       (tLog "Make Pool: " $ createPool (connect config) close 4 60 5)
  --       destroyAllResources
