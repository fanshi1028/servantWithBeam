{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Databases.HitmenBusiness.ErasedMarks
  ( ErasedMark,
    ErasedMarkT (..),
    ErasedMarkId,
    PrimaryKey (ErasedMarkId),
  )
where

import Chronos (Datetime)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Database.Beam (default_, val_, (<-.))
import Database.Beam.Backend (BeamSqlBackend, SqlSerial (SqlSerial))
import Database.Beam.Schema.Tables (Beamable, C, Table (PrimaryKey, primaryKey))
import Databases.HitmenBusiness.Hitmen (HitmanT)
import Databases.HitmenBusiness.Marks (MarkT)
import Databases.HitmenBusiness.Util.Chronos (currentTimestamp_')
import Databases.HitmenBusiness.Util.JSON (flattenBase, noCamelOpt)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (ToSample)
import Typeclass.Base (ToBase (..))

data ErasedMarkB f = ErasedMark
  { _hitmanId :: PrimaryKey HitmanT f,
    _markId :: PrimaryKey MarkT f
  }
  deriving (Generic, Beamable)

data ErasedMarkT f = ErasedMarkAll
  { _erasedMarkId :: C f (SqlSerial Int32),
    _createdAt :: C f Datetime,
    _base :: ErasedMarkB f
  }
  deriving (Generic, Beamable)

type ErasedMark = ErasedMarkT Identity

type ErasedMarkId = PrimaryKey ErasedMarkT Identity

deriving instance Show (ErasedMarkB Identity)

deriving instance Show (ErasedMarkT Identity)

deriving instance Show (PrimaryKey ErasedMarkT Identity)

instance FromHttpApiData (PrimaryKey ErasedMarkT Identity) where
  parseUrlPiece = (ErasedMarkId . SqlSerial <$>) . parseUrlPiece

instance ToHttpApiData (PrimaryKey ErasedMarkT Identity) where
  toUrlPiece (ErasedMarkId (SqlSerial i)) = toUrlPiece i

instance ToJSON (PrimaryKey HitmanT Identity)

instance ToJSON (ErasedMarkB Identity) where
  toJSON = genericToJSON noCamelOpt
  toEncoding = genericToEncoding noCamelOpt

instance ToJSON (ErasedMarkT Identity) where
  toJSON = flattenBase <$> genericToJSON noCamelOpt

instance FromJSON (PrimaryKey HitmanT Identity)

instance FromJSON (ErasedMarkB Identity) where
  parseJSON = genericParseJSON noCamelOpt

instance FromJSON (ErasedMarkT Identity)

instance ToSample (SqlSerial Int32) => ToSample (PrimaryKey ErasedMarkT Identity)

instance (ToSample (PrimaryKey HitmanT f), ToSample (PrimaryKey MarkT f)) => ToSample (ErasedMarkB f)

instance (ToSample (SqlSerial Int32), ToSample Datetime) => ToSample (ErasedMarkT Identity)

instance Table ErasedMarkT where
  data PrimaryKey ErasedMarkT f = ErasedMarkId (C f (SqlSerial Int32)) deriving (Generic, Beamable)
  primaryKey = ErasedMarkId . _erasedMarkId

instance (BeamSqlBackend be) => ToBase be ErasedMarkT where
  type Base ErasedMarkT = ErasedMarkB
  fromBase b =
    ErasedMarkAll
      { _erasedMarkId = default_,
        _base = val_ b,
        _createdAt = currentTimestamp_'
      }
  baseAsUpdate body = (<-. val_ body) . _base
