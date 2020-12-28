{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Databases.HitmenBusiness.Handlers
  ( HandlerT (..),
    HandlerB (..),
    PrimaryKey (HandlerId),
    HandlerId,
  )
where

import Chronos (Datetime)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Generics.Labels ()
import Database.Beam (Nullable)
import Database.Beam.Backend (SqlSerial (SqlSerial))
import Database.Beam.Backend.SQL (BeamSqlBackend, BeamSqlBackendCanSerialize)
import Database.Beam.Query (SqlValable (val_), default_)
import Database.Beam.Schema.Tables (Beamable, C, Table (PrimaryKey, primaryKey))
import Databases.HitmenBusiness.Utils.Chronos (currentTimestamp_')
import Databases.HitmenBusiness.Utils.JSON (noCamelOpt)
import Databases.HitmenBusiness.Utils.Types (Codename)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (ToSample)
import Utils.Meta (Meta (..), WithMetaInfo (..))
import Universum

data HandlerB f = Handler
  { _codename :: C f Codename,
    _dieAt :: C (Nullable f) Datetime
  }
  deriving (Generic, Beamable)

instance
  ( BeamSqlBackend be,
    BeamSqlBackendCanSerialize be Text,
    BeamSqlBackendCanSerialize be (Maybe Datetime)
  ) =>
  Meta be HandlerB
  where
  data MetaInfo HandlerB f = HandlerMetaInfo
    { _handlerId :: C f (SqlSerial Int32),
      _createdAt :: C f Datetime
    }
    deriving (Generic, Beamable)
  addMetaInfo b =
    WithMetaInfo
      { _base = val_ b,
        _metaInfo =
          HandlerMetaInfo
            { _handlerId = default_,
              _createdAt = currentTimestamp_'
            }
      }

type HandlerT = WithMetaInfo HandlerB

type HandlerId = PrimaryKey HandlerT Identity

deriving instance Show (HandlerB Identity)

instance FromHttpApiData HandlerId where
  parseUrlPiece = (HandlerId . SqlSerial <$>) . parseUrlPiece

instance ToHttpApiData HandlerId where
  toUrlPiece (HandlerId (SqlSerial i)) = toUrlPiece i

instance ToJSON (HandlerB Identity) where
  toJSON = genericToJSON noCamelOpt
  toEncoding = genericToEncoding noCamelOpt

instance ToJSON (MetaInfo HandlerB Identity) where
  toJSON = genericToJSON noCamelOpt
  toEncoding = genericToEncoding noCamelOpt

deriving instance Show (MetaInfo HandlerB Identity)

deriving instance Show (PrimaryKey HandlerT Identity)

instance ToJSON (PrimaryKey HandlerT Identity)

instance Table HandlerT where
  data PrimaryKey HandlerT f = HandlerId (C f (SqlSerial Int32)) deriving (Generic, Beamable)
  primaryKey = HandlerId . _handlerId . _metaInfo

instance FromJSON (HandlerB Identity) where
  parseJSON = genericParseJSON noCamelOpt

instance FromJSON (MetaInfo HandlerB Identity) where
  parseJSON = genericParseJSON noCamelOpt

instance FromJSON (PrimaryKey HandlerT Identity)

instance ToSample (SqlSerial Int32) => ToSample (PrimaryKey HandlerT Identity)

instance (ToSample (C f (Maybe Datetime)), ToSample (C f Codename)) => ToSample (HandlerB f)

instance (ToSample (SqlSerial Int32), ToSample Datetime) => ToSample (MetaInfo HandlerB Identity)
