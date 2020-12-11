{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Util.Docs (serveDocs, DocAPI, APIWithDoc) where

import Chronos (Datetime, epoch, timeToDatetime)
import Data.Data (Proxy)
import Data.Int (Int32)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.Beam.Backend (SqlSerial (..))
import Network.HTTP.Types.Status (ok200)
import Network.Wai (responseLBS)
import Servant (HasServer (ServerT), Raw, Server, (:<|>) ((:<|>)))
import Servant.Docs (HasDocs, ToSample (..), docs, markdown, singleSample)
import Servant.Server (Tagged (Tagged))

instance ToSample Datetime where
  toSamples _ = singleSample $ timeToDatetime epoch

instance ToSample Int32 where
  toSamples _ = singleSample 1

deriving newtype instance ToSample (SqlSerial Int32)

type DocAPI api = Raw

type APIWithDoc api = api :<|> DocAPI api

serveDocs :: (HasDocs api) => Proxy api -> ServerT api m -> ServerT (APIWithDoc api) m
serveDocs api server = server :<|> Tagged toDocs
  where
    docsBS = encodeUtf8 . pack . markdown . docs $ api
    toDocs _ res = res $ responseLBS ok200 [("Content-Type", "text/plain")] docsBS
