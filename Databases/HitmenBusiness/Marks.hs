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
import Data.Aeson (FromJSON (parseJSON), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON, withObject, (.:))
import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam (Generic, Identity, Nullable)
import Database.Beam.Backend (SqlSerial (SqlSerial))
import Database.Beam.Backend.SQL (BeamSqlBackend, BeamSqlBackendCanSerialize)
import Database.Beam.Query (SqlValable (val_), default_, (<-.))
import Database.Beam.Schema.Tables (Beamable, C, Table (PrimaryKey, primaryKey))
import Databases.HitmenBusiness.Util.Chronos (currentTimestamp_')
import Databases.HitmenBusiness.Util.JSON (flattenBase, noCamelOpt)
import Databases.HitmenBusiness.Util.Types (FirstName, LastName, MarkDescription, MarkStatus)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Text.Read (Read (..))
import Text.Show (Show (..))
import Typeclass.Base (ToBase (..))
import Prelude (Maybe (..), ($), (.))

data MarkB f = Mark
  { _listBounty :: C f Int32,
    _firstName :: C f FirstName,
    _lastName :: C f LastName,
    _description :: C (Nullable f) MarkDescription,
    _status :: C f MarkStatus
  }
  deriving (Generic, Beamable)

data MarkT f = MarkAll
  { _markId :: C f (SqlSerial Int32),
    _createdAt :: C f Datetime,
    _updatedAt :: C f Datetime,
    _base :: MarkB f
  }
  deriving (Generic, Beamable)

type Mark = MarkB Identity

deriving instance ToJSON (PrimaryKey MarkT Identity)

deriving instance ToJSON MarkStatus

type MarkAll = MarkT Identity

instance ToJSON (MarkB Identity) where
  toJSON = genericToJSON noCamelOpt
  toEncoding = genericToEncoding noCamelOpt

instance ToJSON (MarkT Identity) where
  toJSON = flattenBase <$> genericToJSON noCamelOpt

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
  primaryKey = MarkId . _markId

instance FromJSON (PrimaryKey MarkT Identity)

instance FromJSON (MarkB Identity) where
  -- parseJSON = withObject "MarkB" $ \obj ->
  --   Mark <$> (obj .: "list_bounty") <*> (obj .: "first_name") <*> (obj .: "last_name") <*> (obj .: "description") <*> (obj .: "status")
  parseJSON = genericParseJSON noCamelOpt

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
      { _markId = default_,
        _base = val_ b,
        _createdAt = currentTimestamp_',
        _updatedAt = currentTimestamp_'
      }
  baseAsUpdate body = (<-. val_ body) . _base
