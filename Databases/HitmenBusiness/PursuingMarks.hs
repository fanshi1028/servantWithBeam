{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Databases.HitmenBusiness.PursuingMarks
  ( PursuingMarkT (..),
    PursuingMark,
    PrimaryKey (PursuingMarkId),
  )
where

import Chronos (Datetime)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Int (Int32)
import Database.Beam (Generic, Identity, Nullable, default_, val_, (<-.))
import Database.Beam.Backend (BeamSqlBackend, BeamSqlBackendCanSerialize, SqlSerial (SqlSerial))
import Database.Beam.Schema (Table (..))
import Database.Beam.Schema.Tables (Beamable, C)
import Databases.HitmenBusiness.Hitmen (HitmanT)
import Databases.HitmenBusiness.Marks (MarkT)
import Databases.HitmenBusiness.Util.Chronos (currentTimestamp_')
import Databases.HitmenBusiness.Util.JSON (flattenBase, noCamelOpt)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Typeclass.Base (ToBase (..))
import Prelude (Maybe, Show, (.), (<$>))

data PursuingMarkB f = PursuingMark
  { _hitmanId :: PrimaryKey HitmanT f,
    _markId :: PrimaryKey MarkT f,
    _endAt :: C (Nullable f) Datetime
  }
  deriving (Generic, Beamable)

data PursuingMarkT f = PursuingMarkAll
  { _id :: C f (SqlSerial Int32),
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

instance Table PursuingMarkT where
  data PrimaryKey PursuingMarkT f = PursuingMarkId (C f (SqlSerial Int32)) deriving (Generic, Beamable)
  primaryKey = PursuingMarkId . _id

instance
  ( BeamSqlBackend be,
    BeamSqlBackendCanSerialize be (Maybe Datetime)
  ) =>
  ToBase be PursuingMarkT
  where
  type Base PursuingMarkT = PursuingMarkB
  fromBase b =
    PursuingMarkAll
      { _id = default_,
        _base = val_ b,
        _createdAt = currentTimestamp_'
      }
  baseAsUpdate body = (<-. val_ body) . _base
