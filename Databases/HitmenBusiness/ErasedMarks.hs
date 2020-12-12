{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Databases.HitmenBusiness.ErasedMarks
  ( ErasedMark,
    ErasedMarkT (..),
    ErasedMarkId,
    PrimaryKey (ErasedMarkId),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.Time.LocalTime (LocalTime)
import Database.Beam (Generic, Identity, currentTimestamp_, default_, val_, (<-.))
import Database.Beam.Backend (BeamSqlBackend, BeamSqlBackendCanSerialize, SqlSerial (SqlSerial))
import Database.Beam.Schema.Tables (Beamable, C, Table (PrimaryKey, primaryKey))
import Databases.HitmenBusiness.Hitmen
  ( HitmanT,
  )
import Databases.HitmenBusiness.Marks
  ( MarkT,
  )
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Typeclass.Base (ToBase (..))
import Prelude (Show, (.), (<$>))

data ErasedMarkB f = ErasedMark
  { _hitmanId :: PrimaryKey HitmanT f,
    _markId :: PrimaryKey MarkT f
  }
  deriving (Generic, Beamable)

data ErasedMarkT f = ErasedMarkAll
  { _id :: C f (SqlSerial Int32),
    _createdAt :: C f LocalTime,
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

instance ToJSON (ErasedMarkB Identity)

instance ToJSON (ErasedMarkT Identity)

instance FromJSON (PrimaryKey HitmanT Identity)

instance FromJSON (ErasedMarkB Identity)

instance FromJSON (ErasedMarkT Identity)

instance Table ErasedMarkT where
  data PrimaryKey ErasedMarkT f = ErasedMarkId (C f (SqlSerial Int32)) deriving (Generic, Beamable)
  primaryKey = ErasedMarkId . _id

instance
  ( BeamSqlBackend be
  ) =>
  ToBase be ErasedMarkT
  where
  type Base ErasedMarkT = ErasedMarkB
  fromBase b =
    ErasedMarkAll
      { _id = default_,
        _base = val_ b,
        _createdAt = currentTimestamp_
      }
  baseAsUpdate body = (<-. val_ body) . _base
