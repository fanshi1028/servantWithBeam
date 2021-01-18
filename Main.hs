{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Chronos (stopwatch)
-- import System.Metrics (createCounter)
import Colog (Message, WithLog, defCapacity, logInfo, richMessageAction, usingLoggerT, withBackgroundLogger)
import Control.Concurrent (killThread)
import Database.PostgreSQL.Simple (close, connect)
import Databases.HitmenBusiness (hitmenBusinessDb)
import Network.Wai.Handler.Warp (defaultSettings, exceptionResponseForDebug, runSettings, setBeforeMainLoop, setOnExceptionResponse, setPort)
import Servant.Auth.Server (def, defaultJWTSettings, generateKey)
import Servers (homeApp)
import System.Envy (decodeEnv)
import System.Remote.Monitoring (forkServer, serverThreadId)
import Universum
import UnliftIO (MonadUnliftIO (..), toIO)
import qualified UnliftIO (bracket)
import UnliftIO.Pool (createPool, destroyAllResources)
import Utils.Migration (showMigration)
import Utils.Types (Env (Env))

server :: (With [MonadIO, MonadUnliftIO] m, WithLog env Message m) => m ()
server = do
  requestCount <- newTVarIO 0
  server' <- liftIO $ mkServer requestCount . defaultJWTSettings <$> generateKey
  doWelcome <- setBeforeMainLoop <$> toIO (logInfo $ "listening on port: " <> show @Text port)
  tLog "Get Env: " decodeEnv
    >>= either
      (logInfo . fromString)
      ( `withPool`
          ( (tLog "show Migration: " . showMigration)
              -- >=> (tLog "do Migration: " . doMigration)
              >=> (liftIO . runSettings (settings & doWelcome) . server')
          )
      )
  where
    tLog context io =
      liftIO (stopwatch io)
        >>= \(t, a) -> logInfo (context <> show t) >> return a
    mkServer requestCount jwtCfg  = homeApp . Env richMessageAction def jwtCfg requestCount hitmenBusinessDb
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
main =
  bracket (forkServer "localhost" 8000) (killThread . serverThreadId) $
    const $
      withBackgroundLogger defCapacity richMessageAction $
        flip usingLoggerT $ do
          logInfo "Using Backgroud Loggers"
          server


