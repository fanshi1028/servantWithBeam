{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Controllers.Home (HomeAPI, homeApp)
import Database.Beam.Postgres (connectPostgreSQL, defaultConnectInfo)
import Database.PostgreSQL.Simple (postgreSQLConnectionString)
import Lens.Micro ((&), (.~))
import Network.Wai.Handler.Warp (defaultSettings, exceptionResponseForDebug, runSettings, setOnExceptionResponse, setPort)
import System.Environment (getEnv)
import Util.Migration (doMigration)

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
    settings =
      defaultSettings
        & setPort 6868
        & setOnExceptionResponse exceptionResponseForDebug
