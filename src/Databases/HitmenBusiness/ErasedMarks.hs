{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Databases.HitmenBusiness.ErasedMarks
  ( ErasedMarkT,
    ErasedMarkB (ErasedMark),
    ErasedMarkId,
    PrimaryKey (ErasedMarkId),
  )
where

import Chronos (Datetime)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Database.Beam (default_, val_)
import Database.Beam.Backend (BeamSqlBackend, SqlSerial (SqlSerial))
import Database.Beam.Schema.Tables (Beamable, C, Table (PrimaryKey, primaryKey))
import Databases.HitmenBusiness.Hitmen (HitmanT)
import Databases.HitmenBusiness.Marks (MarkT)
import Databases.HitmenBusiness.Utils.Chronos (currentTimestamp_')
import Databases.HitmenBusiness.Utils.JSON (noCamelOpt)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (ToSample)
import Universum
import Utils.Meta (Meta (..), WithMetaInfo (..))

data ErasedMarkB f = ErasedMark
  { _hitmanId :: PrimaryKey HitmanT f,
    _markId :: PrimaryKey MarkT f
  }
  deriving (Generic, Beamable)

instance
  (BeamSqlBackend be) =>
  Meta be ErasedMarkB
  where
  data MetaInfo ErasedMarkB f = ErasedMarkMetaInfo
    { _erasedMarkId :: C f (SqlSerial Int32),
      _createdAt :: C f Datetime
    }
    deriving (Generic, Beamable)
  addMetaInfo b =
    WithMetaInfo
      { _base = val_ b,
        _metaInfo =
          ErasedMarkMetaInfo
            { _erasedMarkId = default_,
              _createdAt = currentTimestamp_'
            }
      }

type ErasedMarkT = WithMetaInfo ErasedMarkB

type ErasedMarkId = PrimaryKey ErasedMarkT Identity

deriving instance Show (ErasedMarkB Identity)

deriving instance Show (PrimaryKey ErasedMarkT Identity)

instance FromHttpApiData (PrimaryKey ErasedMarkT Identity) where
  parseUrlPiece = (ErasedMarkId . SqlSerial <$>) . parseUrlPiece

instance ToHttpApiData (PrimaryKey ErasedMarkT Identity) where
  toUrlPiece (ErasedMarkId (SqlSerial i)) = toUrlPiece i

instance ToJSON (PrimaryKey HitmanT Identity)

instance ToJSON (ErasedMarkB Identity) where
  toJSON = genericToJSON noCamelOpt
  toEncoding = genericToEncoding noCamelOpt

instance ToJSON (MetaInfo ErasedMarkB Identity) where
  toJSON = genericToJSON noCamelOpt
  toEncoding = genericToEncoding noCamelOpt

instance FromJSON (PrimaryKey HitmanT Identity)

instance FromJSON (ErasedMarkB Identity) where
  parseJSON = genericParseJSON noCamelOpt

instance FromJSON (MetaInfo ErasedMarkB Identity) where
  parseJSON = genericParseJSON noCamelOpt

instance ToSample (C f (SqlSerial Int32)) => ToSample (PrimaryKey ErasedMarkT f)

instance (ToSample (PrimaryKey HitmanT f), ToSample (PrimaryKey MarkT f)) => ToSample (ErasedMarkB f)

instance (ToSample (C f (SqlSerial Int32)), ToSample (C f Datetime)) => ToSample (MetaInfo ErasedMarkB f)

instance Table ErasedMarkT where
  data PrimaryKey ErasedMarkT f = ErasedMarkId (C f (SqlSerial Int32)) deriving (Generic, Beamable)
  primaryKey = ErasedMarkId . _erasedMarkId . _metaInfo
