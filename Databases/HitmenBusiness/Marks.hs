{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Databases.HitmenBusiness.Marks
  ( MarkId,
    MarkT (..),
    MarkB (..),
    PrimaryKey (MarkId),
  )
where

import Chronos (Datetime)
import Data.Aeson (FromJSON (parseJSON), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Database.Beam (Nullable)
import Database.Beam.Backend (SqlSerial (SqlSerial))
import Database.Beam.Backend.SQL (BeamSqlBackend, BeamSqlBackendCanSerialize)
import Database.Beam.Query (SqlValable (val_), default_)
import Database.Beam.Schema.Tables (Beamable, C, Table (PrimaryKey, primaryKey))
import Databases.HitmenBusiness.Utils.Chronos (currentTimestamp_')
import Databases.HitmenBusiness.Utils.JSON (noCamelOpt)
import Databases.HitmenBusiness.Utils.Types (FirstName, LastName, MarkDescription, MarkStatus)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (ToSample)
import Universum
import Utils.Meta (Meta (..), WithMetaInfo (..))

data MarkB f = Mark
  { _listBounty :: C f Int32,
    _firstName :: C f FirstName,
    _lastName :: C f LastName,
    _description :: C (Nullable f) MarkDescription,
    _status :: C f MarkStatus
  }
  deriving (Generic, Beamable)

instance
  ( BeamSqlBackend be,
    BeamSqlBackendCanSerialize be (Maybe MarkDescription),
    BeamSqlBackendCanSerialize be Text,
    BeamSqlBackendCanSerialize be MarkStatus
  ) =>
  Meta be MarkB
  where
  data MetaInfo MarkB f = MarkMetaInfo
    { _markId :: C f (SqlSerial Int32),
      _createdAt :: C f Datetime,
      _updatedAt :: C f Datetime
    }
    deriving (Generic, Beamable)
  addMetaInfo b =
    WithMetaInfo
      { _base = val_ b,
        _metaInfo =
          MarkMetaInfo
            { _markId = default_,
              _createdAt = currentTimestamp_',
              _updatedAt = currentTimestamp_'
            }
      }

type MarkT = WithMetaInfo MarkB

deriving instance ToJSON (PrimaryKey MarkT Identity)

deriving instance ToJSON MarkStatus

instance ToJSON (MarkB Identity) where
  toJSON = genericToJSON noCamelOpt
  toEncoding = genericToEncoding noCamelOpt

instance ToJSON (MetaInfo MarkB Identity) where
  toJSON = genericToJSON noCamelOpt
  toEncoding = genericToEncoding noCamelOpt

type MarkId = PrimaryKey MarkT Identity

instance FromHttpApiData MarkId where
  parseUrlPiece = (MarkId . SqlSerial <$>) . parseUrlPiece

instance ToHttpApiData MarkId where
  toUrlPiece (MarkId (SqlSerial i)) = toUrlPiece i

deriving instance Show (MarkB Identity)

deriving instance Show (PrimaryKey MarkT Identity)

deriving instance Read (PrimaryKey MarkT Identity)

instance Table MarkT where
  data PrimaryKey MarkT f = MarkId (C f (SqlSerial Int32)) deriving (Generic, Beamable)
  primaryKey = MarkId . _markId . _metaInfo

instance FromJSON (PrimaryKey MarkT Identity)

instance FromJSON (MarkB Identity) where
  parseJSON = genericParseJSON noCamelOpt

instance FromJSON (MetaInfo MarkB Identity) where
  parseJSON = genericParseJSON noCamelOpt

instance ToSample (C f (SqlSerial Int32)) => ToSample (PrimaryKey MarkT f)

instance
  ( ToSample (C f (Maybe Datetime)),
    ToSample (C f Int32),
    ToSample (C f FirstName),
    ToSample (C f LastName),
    ToSample (C f (Maybe MarkDescription)),
    ToSample (C f MarkStatus)
  ) =>
  ToSample (MarkB f)

instance
  ( ToSample (C f Int32),
    ToSample (C f(SqlSerial Int32)),
    ToSample (C f Datetime)
  ) =>
  ToSample (MetaInfo MarkB f)
