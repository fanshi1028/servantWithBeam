{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Chronos (stopwatch)
import Colog (HasLog (..), WithLog, logMsg, logTextStdout, usingLoggerT)
import Control.Concurrent (killThread)
import Database.PostgreSQL.Simple (close, connect, connectPostgreSQL)
import Network.Wai.Handler.Warp (defaultSettings, exceptionResponseForDebug, runSettings, setBeforeMainLoop, setOnExceptionResponse, setPort)
import Servant (Context (EmptyContext, (:.)))
import Servant.Auth.Server (def, defaultCookieSettings, defaultJWTSettings, generateKey)
import Servers (homeApp)
import System.Envy (decodeEnv)
import System.Remote.Monitoring (forkServer, serverThreadId)
import Universum
import UnliftIO (MonadUnliftIO (..), toIO)
import qualified UnliftIO (bracket)
import UnliftIO.Pool (createPool, destroyAllResources)
import Utils.Migration (doMigration, showMigration)

server :: (With [MonadIO, MonadUnliftIO] m, WithLog env Text m) => m ()
server = do
  server <- liftIO $ mkServer . defaultJWTSettings <$> generateKey
  doWelcome <- setBeforeMainLoop <$> toIO (log $ "listening on port: " <> show @Text port)
  tLog "Get Env: " decodeEnv
    >>= either (log . fromString) (`withPool` (liftIO . runSettings (settings & doWelcome) . server))
  where
    log = logMsg @Text
    tLog context io =
      liftIO (stopwatch io)
        >>= \(t, a) -> log (context <> show t) >> return a
    mkServer jwtCfg = homeApp (defaultCookieSettings :. jwtCfg :. EmptyContext) def jwtCfg
    withPool config =
      UnliftIO.bracket
        (tLog "Make Pool: " $ createPool (connect config) close 6 60 10)
        destroyAllResources
    settings =
      defaultSettings
        & setPort port
        & setOnExceptionResponse exceptionResponseForDebug
    port = 6868

main :: IO ()
main = bracket (forkServer "localhost" 8000) (killThread . serverThreadId) $ const $ usingLoggerT logTextStdout server

-- ( makePool
--     -- >=> tLog "show Migration: " . showMigration
--     -- >=> tLog "show Migration: " . doMigration
--     >=> runSettings settings . homeApp cfg def jwtCfg
-- )

-- bracket (forkServer "localhost" 8000) (killThread . serverThreadId) $ const $ usingLoggerT (cmap fmtMessage logTextStdout) server
