{-# LANGUAGE OverloadedLabels #-}

module Main where

import Chronos (stopwatch)
import Database.PostgreSQL.Simple (connectPostgreSQL, postgreSQLConnectionString)
import Network.Wai.Handler.Warp (defaultSettings, exceptionResponseForDebug, runSettings, setBeforeMainLoop, setOnExceptionResponse, setPort)
import Servant (Context (EmptyContext, (:.)))
import Servant.Auth.Server (def, defaultCookieSettings, defaultJWTSettings, generateKey)
import Servers (homeApp)
import System.Envy (decodeEnv)
import Universum
import Utils.Migration (doMigration, showMigration)

main :: IO ()
main = do
  key <- generateKey
  let jwtCfg = defaultJWTSettings key
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
  decodeEnv'
    >>= either
      putStrLn
      ( connectDb
          -- >=> tLog "show Migration: " . showMigration
          -- >=> tLog "show Migration: " . doMigration
          >=> runSettings settings . homeApp cfg def jwtCfg
      )
  where
    logTarget = stderr
    log = hPutStrLn @Text @IO logTarget
    tLog context io = stopwatch io >>= \(t, a) -> log (context <> show t) >> return a
    decodeEnv' = tLog "Get Env: " decodeEnv
    connectDb = tLog "Connect DB: " . connectPostgreSQL . postgreSQLConnectionString
    port = 6868
    settings =
      defaultSettings
        & setPort port
        & setOnExceptionResponse exceptionResponseForDebug
        & setBeforeMainLoop (log $ "listening on port: " <> show @Text port)

-- main = connectDb >>= run 6868 . homeApp
