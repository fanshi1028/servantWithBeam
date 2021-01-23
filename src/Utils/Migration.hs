module Utils.Migration
  ( showAndDoMigration,
    doMigration,
    showMigration1,
    showMigration,
  )
where

import Database.Beam.AutoMigrate (migrate, printMigration, tryRunMigrationsWithEditUpdate)
import Database.Beam.AutoMigrate.Postgres (getSchema)
import Database.Beam.Postgres (Connection, runBeamPostgresDebug)
import Database.PostgreSQL.Simple (withTransaction)
import Databases.HitmenBusiness (annotatedHitmenBusinessDb, hitmenBusinessDbSchema)
import Universum
import UnliftIO.Pool (Pool, withResource)

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
