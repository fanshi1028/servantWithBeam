{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Chronos (stopwatch)
import Colog (Message, WithLog, defCapacity, logInfo, richMessageAction, usingLoggerT, withBackgroundLogger)
import Control.Arrow (ArrowChoice ((|||)))
import Control.Concurrent (killThread)
import Database.PostgreSQL.Simple (close, connect)
import Databases.HitmenBusiness (hitmenBusinessDb)
import Network.Wai.Handler.Warp (defaultSettings, exceptionResponseForDebug, runSettings, setBeforeMainLoop, setOnExceptionResponse, setPort)
import Network.Wai.Middleware.Servant.Errors (errorMwDefJson)
import Servant.Auth.Server (def, defaultJWTSettings, generateKey)
import Servers (homeApp)
import System.Envy (decodeEnv)
import System.Metrics.Counter (Counter)
import System.Remote.Monitoring (forkServer, getCounter, serverThreadId)
import Universum
import UnliftIO (MonadUnliftIO (..), toIO)
import qualified UnliftIO (bracket)
import UnliftIO.Pool (createPool, destroyAllResources)
import Utils.Migration (showMigration)
import Utils.Types (Env (Env))

server :: (With [MonadIO, MonadUnliftIO] m, WithLog env Message m) => Counter -> m ()
server ekgCounter = do
  requestCount <- newTVarIO 0
  server' <- liftIO $ mkServer requestCount . defaultJWTSettings <$> generateKey
  doWelcome <- setBeforeMainLoop <$> toIO (logInfo $ "listening on port: " <> show @Text port)
  tLog "Get Env: " decodeEnv
    >>= logInfo . fromString
      ||| flip
        withPool
        ( tLog "show Migration: " . showMigration
            -- >=> (tLog "do Migration: " . doMigration)
            >=> liftIO . runSettings (settings & doWelcome) . errorMwDefJson . server'
        )
  where
    tLog context io =
      liftIO (stopwatch io)
        >>= \(t, a) -> logInfo (context <> show t) >> return a
    mkServer requestCount jwtCfg = homeApp . Env richMessageAction def jwtCfg requestCount ekgCounter hitmenBusinessDb
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
  bracket (forkServer "localhost" 8000) (killThread . serverThreadId) $ \ekg ->
    withBackgroundLogger defCapacity richMessageAction $
      flip usingLoggerT $ do
        logInfo "Initialized the ekg counter"
        counter <- liftIO $ getCounter "counter" ekg
        logInfo "Using Backgroud Loggers"
        server counter

