{-# LANGUAGE OverloadedLabels #-}

module Main where

import Database.Beam.Postgres (connectPostgreSQL, defaultConnectInfo)
import Database.PostgreSQL.Simple (postgreSQLConnectionString)
import Network.Wai.Handler.Warp (defaultSettings, exceptionResponseForDebug, runSettings, setBeforeMainLoop, setOnExceptionResponse, setPort)
import Servers (homeApp)
import System.Environment (getEnv)
import Util.Migration (doMigration, showMigration)

connectDb user db =
  connectPostgreSQL
    ( postgreSQLConnectionString $
        defaultConnectInfo
          & #connectUser .~ user
          & #connectDatabase .~ db
    )

main :: IO ()
-- showMigration >>=
-- main = connectDb >>= doMigration >>= run 6868 . homeApp

-- main = connectDb >>= run 6868 . homeApp

main =
  join
    ( connectDb
        <$> getEnv "PG_USER"
        <*> getEnv "HITMEN_DB"
    )
    >>= doMigration
    >>= runSettings settings . homeApp
  where
    port = 6868
    settings =
      defaultSettings
        & setPort port
        & setOnExceptionResponse exceptionResponseForDebug
        & setBeforeMainLoop (hPutStrLn stderr $ "listening on port: " <> show @Text port)

-- main = connectDb >>= run 6868 . homeApp
