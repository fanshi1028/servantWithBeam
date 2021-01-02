{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Docs (serveDocs, DocAPI, APIWithDoc) where

import Chronos (Datetime, epoch, timeToDatetime)
import Database.Beam.Backend (SqlSerial (..))
import Network.HTTP.Types (ok200)
import Network.Wai (responseLBS)
import Servant (HasServer (ServerT), Raw, Tagged (Tagged), (:<|>) ((:<|>)), (:>))
import Servant.Docs (HasDocs, ToSample (..), docs, markdown, singleSample)
import Universum
import Servant.Auth (Cookie)

instance ToSample Datetime where
  toSamples _ = singleSample $ timeToDatetime epoch

instance ToSample Int32 where
  toSamples _ = singleSample 1

deriving newtype instance ToSample (SqlSerial Int32)

type DocAPI api = "docs" :> Raw

type APIWithDoc api = api :<|> DocAPI api

serveDocs :: (HasDocs api) => Proxy api -> ServerT api m -> ServerT (APIWithDoc api) m
serveDocs api server = server :<|> Tagged toDocs
  where
    docsBS = encodeUtf8 . markdown . docs $ api
    toDocs _ res = res $ responseLBS ok200 [("Content-Type", "text/plain")] docsBS

-- instance ToCapture
