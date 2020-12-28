{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Databases.HitmenBusiness.Hitmen
  ( HitmanT (..),
    HitmanB (..),
    HitmanId,
    PrimaryKey (HitmanId),
  )
where

import Chronos (Datetime)
import Data.Aeson (FromJSON (parseJSON), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Database.Beam (Nullable, default_, val_)
import Database.Beam.Backend (SqlSerial (SqlSerial))
import Database.Beam.Backend.SQL (BeamSqlBackend, BeamSqlBackendCanSerialize)
import Database.Beam.Schema.Tables (Beamable, C, Table (PrimaryKey, primaryKey))
import Databases.HitmenBusiness.Handlers (HandlerT, PrimaryKey (HandlerId))
import Databases.HitmenBusiness.Utils.Chronos (currentTimestamp_')
import Databases.HitmenBusiness.Utils.JSON (noCamelOpt)
import Databases.HitmenBusiness.Utils.Types (Codename)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (ToSample)
import Utils.Meta (Meta (..), WithMetaInfo (..))
import Universum

data HitmanB f = Hitman
  { _codename :: C f Codename,
    _dieAt :: C (Nullable f) Datetime
  }
  deriving (Generic, Beamable)

instance
  ( BeamSqlBackend be,
    BeamSqlBackendCanSerialize be Text,
    BeamSqlBackendCanSerialize be (Maybe Datetime)
  ) =>
  Meta be HitmanB
  where
  data MetaInfo HitmanB f = HitmanMetaInfo
    { _hitmanId :: C f (SqlSerial Int32),
      _handlerId :: PrimaryKey HandlerT f,
      _createdAt :: C f Datetime
    }
    deriving (Generic, Beamable)
  addMetaInfo b =
    WithMetaInfo
      { _base = val_ b,
        _metaInfo =
          HitmanMetaInfo
            { _hitmanId = default_,
              _createdAt = currentTimestamp_',
              -- _handlerId = val_ hid
              _handlerId = val_ (HandlerId 1) --  TEMP TEMP
            }
      }

type HitmanT = WithMetaInfo HitmanB

instance ToJSON (HitmanB Identity) where
  toJSON = genericToJSON noCamelOpt
  toEncoding = genericToEncoding noCamelOpt

instance ToJSON (MetaInfo HitmanB Identity) where
  toJSON = genericToJSON noCamelOpt
  toEncoding = genericToEncoding noCamelOpt

instance FromHttpApiData HitmanId where
  parseUrlPiece = (HitmanId . SqlSerial <$>) . parseUrlPiece

instance ToHttpApiData HitmanId where
  toUrlPiece (HitmanId (SqlSerial i)) = toUrlPiece i

type HitmanId = PrimaryKey HitmanT Identity

deriving instance Show (HitmanB Identity)

deriving instance Show HitmanId

instance Table HitmanT where
  data PrimaryKey HitmanT f = HitmanId (C f (SqlSerial Int32)) deriving (Generic, Beamable)
  primaryKey = HitmanId . _hitmanId . _metaInfo

instance FromJSON (HitmanB Identity) where
  parseJSON = genericParseJSON noCamelOpt

instance FromJSON (MetaInfo HitmanB Identity) where
  parseJSON = genericParseJSON noCamelOpt

instance ToSample (SqlSerial Int32) => ToSample (PrimaryKey HitmanT Identity)

instance (ToSample (C f (Maybe Datetime)), ToSample (C f Codename)) => ToSample (HitmanB f)

instance
  ( ToSample (PrimaryKey HandlerT Identity),
    ToSample (SqlSerial Int32),
    ToSample Datetime
  ) =>
  ToSample (MetaInfo HitmanB Identity)
