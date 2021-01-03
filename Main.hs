{-# LANGUAGE OverloadedLabels #-}

module Main where

import Chronos (stopwatch)
import Database.Beam.Postgres (connectPostgreSQL, defaultConnectInfo)
import Database.PostgreSQL.Simple (postgreSQLConnectionString)
import Network.Wai.Handler.Warp (defaultSettings, exceptionResponseForDebug, runSettings, setBeforeMainLoop, setOnExceptionResponse, setPort)
import Servant (Context (EmptyContext, (:.)))
import Servant.Auth.Server (def, defaultCookieSettings, defaultJWTSettings, generateKey)
import Servers (homeApp)
import System.Environment (getEnv)
import Universum
import Utils.Migration (doMigration, showMigration)

connectDb' user db =
  connectPostgreSQL
    ( postgreSQLConnectionString $
        defaultConnectInfo
          & #connectUser .~ user
          & #connectDatabase .~ db
    )

main :: IO ()
main = do
  -- connectDb
  -- >>= tLog "show Migration: " . showMigration
  -- >>= runSettings settings . homeApp
  key <- generateKey
  let jwtCfg = defaultJWTSettings key
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
  connectDb
    -- >>= tLog "show Migration: " . showMigration
    -- >>= tLog "show Migration: " . doMigration
    >>= runSettings settings . homeApp cfg def jwtCfg
  where
    logTarget = stderr
    log = hPutStrLn @Text @IO logTarget
    tLog context io = stopwatch io >>= \(t, a) -> log (context <> show t) >> return a
    envVar' s = log ("Try get " <> s) >> getEnv (toString s) <* log ("Got " <> s)
    envVar s = tLog ("envVar " <> s <> ": ") $ envVar' s
    connectDb'' = tLog "Connect DB: " <<$>> connectDb'
    connectDb = log "Try Connect DB" >> join (connectDb'' <$> envVar "PG_USER" <*> envVar "HITMEN_DB") <* log "Connected"
    port = 6868
    settings =
      defaultSettings
        & setPort port
        & setOnExceptionResponse exceptionResponseForDebug
        & setBeforeMainLoop (log $ "listening on port: " <> show @Text port)

-- main = connectDb >>= run 6868 . homeApp
