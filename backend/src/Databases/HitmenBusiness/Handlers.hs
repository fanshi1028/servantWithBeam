{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Databases.HitmenBusiness.Handlers
  ( HandlerT,
    HandlerB (Handler),
    PrimaryKey (HandlerId),
    HandlerId,
  )
where

import Chronos (Datetime)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Generics.Labels ()

import Data.Validity (prettyValidate, Validity)
import Database.Beam (HasSqlEqualityCheck, Nullable)
import Database.Beam.Backend (HasSqlValueSyntax, SqlSerial (SqlSerial))
import Database.Beam.Backend.SQL (BeamSqlBackend, BeamSqlBackendCanSerialize)
import Database.Beam.Query (SqlValable (val_), default_)
import Database.Beam.Schema.Tables (Beamable, C, Table (PrimaryKey, primaryKey))
import Databases.HitmenBusiness.Utils.Chronos (currentTimestamp_')
import Databases.HitmenBusiness.Utils.JSON (noCamelOpt)
import Databases.HitmenBusiness.Utils.Types (Codename)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Auth.JWT (FromJWT, ToJWT)
import Servant.Docs (ToSample)
import Universum
import Utils.Account.Login (LoginId)
import Utils.Account.SignUp (Payload)
import Utils.Meta (Meta (..), WithMetaInfo (..))

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

instance Validity (Payload HandlerB)

instance Validity (HandlerB Identity)

instance FromJSON (HandlerB Identity) where
  parseJSON = genericParseJSON noCamelOpt >=> either fail return . prettyValidate

instance FromJSON (MetaInfo HandlerB Identity) where
  parseJSON = genericParseJSON noCamelOpt

instance FromJSON (PrimaryKey HandlerT Identity)

instance ToSample (C f (SqlSerial Int32)) => ToSample (PrimaryKey HandlerT f)

instance (ToSample (C f (Maybe Datetime)), ToSample (C f Codename)) => ToSample (HandlerB f)

instance (ToSample (C f (SqlSerial Int32)), ToSample (C f Datetime)) => ToSample (MetaInfo HandlerB f)

instance FromJWT (WithMetaInfo HandlerB Identity)

instance ToJWT (WithMetaInfo HandlerB Identity)

newtype instance LoginId HandlerB = HandlerLoginId {unHandlerLoginId :: Codename}
  deriving newtype (ToSample, FromJSON, ToJSON, Validity)

deriving newtype instance (HasSqlValueSyntax syntax Text) => HasSqlValueSyntax syntax (LoginId HandlerB)

deriving newtype instance (BeamSqlBackend be, HasSqlEqualityCheck be Codename) => HasSqlEqualityCheck be (LoginId HandlerB)
