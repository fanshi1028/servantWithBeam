{-# LANGUAGE OverloadedLabels #-}

module Main where

import Chronos (stopwatch)
import Control.Concurrent (killThread)
import Data.Pool (createPool, destroyAllResources)
import Database.PostgreSQL.Simple (close, connect, connectPostgreSQL)
import Network.Wai.Handler.Warp (defaultSettings, exceptionResponseForDebug, runSettings, setBeforeMainLoop, setOnExceptionResponse, setPort)
import Servant (Context (EmptyContext, (:.)))
import Servant.Auth.Server (def, defaultCookieSettings, defaultJWTSettings, generateKey)
import Servers (homeApp)
import System.Envy (decodeEnv)
import System.Remote.Monitoring (forkServer, serverThreadId)
import Universum
import Utils.Migration (doMigration, showMigration)

server :: IO ()
server = do
  key <- generateKey
  let jwtCfg = defaultJWTSettings key
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      runServer = runSettings settings . homeApp cfg def jwtCfg
      runServerWithPool info = bracket (makePool info) destroyAllResources runServer
  tLog "Get Env: " decodeEnv >>= either putStrLn runServerWithPool
  where
    -- ( makePool
    --     -- >=> tLog "show Migration: " . showMigration
    --     -- >=> tLog "show Migration: " . doMigration
    --     >=> runSettings settings . homeApp cfg def jwtCfg
    -- )

    logTarget = stderr
    log = hPutStrLn @Text @IO logTarget
    tLog context io = stopwatch io >>= \(t, a) -> log (context <> show t) >> return a
    makePool config = tLog "Make Pool: " $ createPool (tLog "Connect DB: " $ connect config) close 6 60 10
    port = 6868
    settings =
      defaultSettings
        & setPort port
        & setOnExceptionResponse exceptionResponseForDebug
        & setBeforeMainLoop (log $ "listening on port: " <> show @Text port)

main :: IO ()
main = do
  bracket (forkServer "localhost" 8000) (killThread . serverThreadId) $ const server
