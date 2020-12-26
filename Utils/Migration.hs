module Utils.Migration
  ( showAndDoMigration,
    doMigration,
    showMigration1,
    showMigration,
  )
where

import Database.Beam.AutoMigrate (migrate, printMigration, tryRunMigrationsWithEditUpdate)
import Database.Beam.AutoMigrate.Postgres (getSchema)
import Database.Beam.Postgres (runBeamPostgresDebug)
import Database.PostgreSQL.Simple (withTransaction)
import Databases.HitmenBusiness (annotatedHitmenBusinessDb, hitmenBusinessDbSchema)
import Universum

showMigration conn = do
  withTransaction conn $
    runBeamPostgresDebug putStrLn conn $
      printMigration $
        migrate conn hitmenBusinessDbSchema
  return conn

showMigration1 conn = do
  actualSchema <- getSchema conn
  -- print $ diff actualSchema hitmenBusinessDbSchema
  -- putStrLn $ "======================================================"
  -- print $ hitmenBusinessDbSchema
  print actualSchema
  -- print $ diff hitmenBusinessDbSchema actualSchema
  return conn

-- doMigration = tryRunMigrationsWithEditUpdate annotatedHitmenBusinessDb
doMigration conn = do
  tryRunMigrationsWithEditUpdate annotatedHitmenBusinessDb conn
  putTextLn "Migration done"
  return conn

showAndDoMigration conn = do
  showMigration conn
  putTextLn "migrate?"
  getLine >>= \case
    "y" -> doMigration conn
    "Y" -> doMigration conn
    _ -> putTextLn "Exiting" >> return conn
