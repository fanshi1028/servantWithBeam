{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Databases.HitmenBusiness.PursuingMarks
  ( PursuingMarkT (..),
    PursuingMark,
    PrimaryKey (PursuingMarkId),
  )
where

import Chronos (Datetime)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Database.Beam (Nullable, default_, val_, (<-.))
import Database.Beam.Backend (BeamSqlBackend, BeamSqlBackendCanSerialize, SqlSerial (SqlSerial))
import Database.Beam.Schema (Table (..))
import Database.Beam.Schema.Tables (Beamable, C)
import Databases.HitmenBusiness.Hitmen (HitmanT)
import Databases.HitmenBusiness.Marks (MarkT)
import Databases.HitmenBusiness.Utils.Chronos (currentTimestamp_')
import Databases.HitmenBusiness.Utils.JSON (flattenBase, noCamelOpt)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (ToSample)
import Typeclass.Base (ToBase (..))
import Universum

data PursuingMarkB f = PursuingMark
  { _hitmanId :: PrimaryKey HitmanT f,
    _markId :: PrimaryKey MarkT f,
    _endAt :: C (Nullable f) Datetime
  }
  deriving (Generic, Beamable)

data PursuingMarkT f = PursuingMarkAll
  { _pursuingMarkId :: C f (SqlSerial Int32),
    _createdAt :: C f Datetime,
    _base :: PursuingMarkB f
  }
  deriving (Generic, Beamable)

type PursuingMark = PursuingMarkT Identity

deriving instance Show (PursuingMarkB Identity)

deriving instance Show (PursuingMarkT Identity)

deriving instance Show (PrimaryKey PursuingMarkT Identity)

instance FromHttpApiData (PrimaryKey PursuingMarkT Identity) where
  parseUrlPiece = (PursuingMarkId . SqlSerial <$>) . parseUrlPiece

instance ToHttpApiData (PrimaryKey PursuingMarkT Identity) where
  toUrlPiece (PursuingMarkId (SqlSerial i)) = toUrlPiece i

instance ToJSON (PrimaryKey HitmanT Identity)

instance ToJSON (PursuingMarkB Identity) where
  toJSON = genericToJSON noCamelOpt
  toEncoding = genericToEncoding noCamelOpt

instance ToJSON (PursuingMarkT Identity) where
  toJSON = flattenBase <$> genericToJSON noCamelOpt

instance FromJSON (PursuingMarkT Identity)

instance FromJSON (PrimaryKey HitmanT Identity)

instance FromJSON (PursuingMarkB Identity) where
  parseJSON = genericParseJSON noCamelOpt

instance ToSample (SqlSerial Int32) => ToSample (PrimaryKey PursuingMarkT Identity)

instance
  ( ToSample (C f (Maybe Datetime)),
    ToSample (PrimaryKey HitmanT f),
    ToSample (PrimaryKey MarkT f)
  ) =>
  ToSample (PursuingMarkB f)

instance (ToSample (SqlSerial Int32), ToSample Datetime) => ToSample (PursuingMarkT Identity)

instance Table PursuingMarkT where
  data PrimaryKey PursuingMarkT f = PursuingMarkId (C f (SqlSerial Int32)) deriving (Generic, Beamable)
  primaryKey = PursuingMarkId . _pursuingMarkId

instance
  ( BeamSqlBackend be,
    BeamSqlBackendCanSerialize be (Maybe Datetime)
  ) =>
  ToBase be PursuingMarkT
  where
  type Base PursuingMarkT = PursuingMarkB
  fromBase b =
    PursuingMarkAll
      { _pursuingMarkId = default_,
        _base = val_ b,
        _createdAt = currentTimestamp_'
      }
  baseAsUpdate body = (<-. val_ body) . _base
