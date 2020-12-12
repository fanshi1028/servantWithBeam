{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Databases.HitmenBusiness.Handlers
  ( HandlerT (..),
    HandlerB (..),
    Handler,
    PrimaryKey (HandlerId),
    HandlerAll,
    HandlerId,
  )
where

import Chronos (Datetime)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON, (.:))
import Data.Generics.Labels ()
import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam (Generic, Identity, Nullable)
import Database.Beam.Backend (SqlSerial (SqlSerial))
import Database.Beam.Backend.SQL (BeamSqlBackend, BeamSqlBackendCanSerialize)
import Database.Beam.Query (SqlValable (val_), default_, (<-.))
import Database.Beam.Schema.Tables (Beamable, C, Table (PrimaryKey, primaryKey))
import Databases.HitmenBusiness.Util.Chronos (currentTimestamp_')
import Databases.HitmenBusiness.Util.JSON (flattenBase, noCamelOpt)
import Databases.HitmenBusiness.Util.Types (Codename)
import Servant (Capture, FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (ToCapture, ToSample)
import Typeclass.Base (ToBase (..))
import Prelude (Maybe, Show, (.), (<$>))

data HandlerB f = Handler
  { _codename :: C f Codename,
    _dieAt :: C (Nullable f) Datetime
  }
  deriving (Generic, Beamable)

data HandlerT f = HandlerAll
  { _handlerId :: C f (SqlSerial Int32),
    _base :: HandlerB f,
    _createdAt :: C f Datetime
  }
  deriving (Generic, Beamable)

type HandlerAll = HandlerT Identity

type Handler = HandlerB Identity

type HandlerId = PrimaryKey HandlerT Identity

deriving instance Show (HandlerB Identity)

instance FromHttpApiData HandlerId where
  parseUrlPiece = (HandlerId . SqlSerial <$>) . parseUrlPiece

instance ToHttpApiData HandlerId where
  toUrlPiece (HandlerId (SqlSerial i)) = toUrlPiece i

deriving instance Show (HandlerT Identity)

instance ToJSON (HandlerB Identity) where
  toJSON = genericToJSON noCamelOpt
  toEncoding = genericToEncoding noCamelOpt

instance ToJSON (HandlerT Identity) where
  toJSON = flattenBase <$> genericToJSON noCamelOpt

deriving instance Show (PrimaryKey HandlerT Identity)

instance ToJSON (PrimaryKey HandlerT Identity)

instance Table HandlerT where
  data PrimaryKey HandlerT f = HandlerId (C f (SqlSerial Int32)) deriving (Generic, Beamable)
  primaryKey = HandlerId . _handlerId

instance FromJSON (HandlerB Identity) where
  parseJSON = genericParseJSON noCamelOpt

instance FromJSON (HandlerT Identity)

instance FromJSON (PrimaryKey HandlerT Identity)

instance ToSample (SqlSerial Int32) => ToSample (PrimaryKey HandlerT Identity)

instance (ToSample (C f (Maybe Datetime)), ToSample (C f Codename)) => ToSample (HandlerB f)

instance (ToSample (SqlSerial Int32), ToSample Datetime) => ToSample (HandlerT Identity)

instance
  ( BeamSqlBackend be,
    BeamSqlBackendCanSerialize be Text,
    BeamSqlBackendCanSerialize be (Maybe Datetime)
  ) =>
  ToBase be HandlerT
  where
  type Base HandlerT = HandlerB
  fromBase b =
    HandlerAll
      { _handlerId = default_,
        _base = val_ b,
        _createdAt = currentTimestamp_'
      }
  baseAsUpdate body = (<-. val_ body) . _base

-- instance
--   ( BeamSqlBackend be,
--     BeamSqlBackendCanSerialize be Text,
--     BeamSqlBackendCanSerialize be UTCTime
--   ) =>
--   FromJSON (HandlerT (QExpr be s))
--   where
--   parseJSON = withObject "" $ \obj ->
--     HandlerAll
--       <$> (maybe default_ val_ <$> obj .:? "id")
--       <*> (Handler <$> (val_ <$> obj .: "codename") <*> (val_ <$> obj .: "dieAt"))
--       <*> (val_ <$> obj .: "createdAt")
