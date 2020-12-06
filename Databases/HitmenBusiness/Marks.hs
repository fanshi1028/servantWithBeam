{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Databases.HitmenBusiness.Marks
  ( Mark,
    MarkAll,
    MarkId,
    MarkT (MarkAll),
    MarkB (Mark),
    PrimaryKey (MarkId),
  )
where

import Chronos (Datetime)
import Control.Applicative (Applicative ((<*>)), (<$>))
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON (parseJSON), withObject, (.:))
import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam (Generic, Identity, Nullable, Typeable)
import Database.Beam.AutoMigrate (HasColumnType (..), PgEnum)
import Database.Beam.Backend (SqlSerial (SqlSerial))
import Database.Beam.Backend.SQL (BeamSqlBackend, BeamSqlBackendCanSerialize, HasSqlValueSyntax (..), autoSqlValueSyntax)
import Database.Beam.Backend.SQL.Row (FromBackendRow (..))
import Database.Beam.Backend.Types (BeamBackend)
import Database.Beam.Query (SqlValable (val_), default_, (<-.))
import Database.Beam.Schema.Tables (Beamable, C, Table (PrimaryKey, primaryKey))
import Databases.HitmenBusiness.Util.Chronos (currentTimestamp_')
import Databases.HitmenBusiness.Util.Types (FirstName, LastName, MarkDescription, MarkStatus)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Text.Read (Read (..), readMaybe)
import Text.Show (Show (..))
import Typeclass.Base (ToBase (..))
import Prelude (Bounded, Enum, Maybe (..), String, fail, maybe, return, ($), (.), (>>=))

instance FromJSON (PrimaryKey MarkT Identity)


data MarkB f = Mark
  { _listBounty :: C f Int32,
    _firstName :: C f FirstName,
    _lastName :: C f LastName,
    _description :: C (Nullable f) MarkDescription,
    _status :: C f MarkStatus
  }
  deriving (Generic, Beamable)

data MarkT f = MarkAll
  { _mid :: C f (SqlSerial Int32),
    _createdAt :: C f Datetime,
    _updatedAt :: C f Datetime,
    _base :: MarkB f
  }
  deriving (Generic, Beamable)

type Mark = MarkB Identity

deriving instance ToJSON (PrimaryKey MarkT Identity)

deriving instance ToJSON MarkStatus

deriving instance ToJSON (MarkB Identity)

type MarkAll = MarkT Identity

deriving instance ToJSON (MarkT Identity)

type MarkId = PrimaryKey MarkT Identity

instance FromHttpApiData MarkId where
  parseUrlPiece = (MarkId . SqlSerial <$>) . parseUrlPiece

instance ToHttpApiData MarkId where
  toUrlPiece (MarkId (SqlSerial i)) = toUrlPiece i

deriving instance Show (MarkB Identity)

deriving instance Show (MarkT Identity)

deriving instance Show (PrimaryKey MarkT Identity)

deriving instance Read (PrimaryKey MarkT Identity)

instance Table MarkT where
  data PrimaryKey MarkT f = MarkId (C f (SqlSerial Int32)) deriving (Generic, Beamable)
  primaryKey = MarkId . _mid

instance FromJSON (MarkB Identity) where
  parseJSON = withObject "MarkB" $ \obj ->
    Mark <$> (obj .: "list_bounty") <*> (obj .: "first_name") <*> (obj .: "last_name") <*> (obj .: "description") <*> (obj .: "status")

instance FromJSON (MarkT Identity)

instance
  ( BeamSqlBackend be,
    BeamSqlBackendCanSerialize be (Maybe MarkDescription),
    BeamSqlBackendCanSerialize be Text,
    BeamSqlBackendCanSerialize be MarkStatus
  ) =>
  ToBase be MarkT
  where
  type Base MarkT = MarkB
  fromBase b =
    MarkAll
      { _mid = default_,
        _base = val_ b,
        _createdAt = currentTimestamp_',
        _updatedAt = currentTimestamp_'
      }
  baseAsUpdate body = (<-. val_ body) . _base
