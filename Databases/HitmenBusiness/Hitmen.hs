{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Databases.HitmenBusiness.Hitmen
  ( HitmanT (..),
    HitmanB (..),
    Hitman,
    HitmanAll,
    HitmanId,
    PrimaryKey (HitmanId),
  )
where

import Chronos (Datetime)
import Control.Applicative (Applicative (liftA2, (<*>)), (<$>))
import Data.Aeson (FromJSON (parseJSON), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON, withObject, (.:))
import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam (Generic, Identity, Nullable, default_, val_, (<-.))
import Database.Beam.Backend (SqlSerial (SqlSerial))
import Database.Beam.Backend.SQL (BeamSqlBackend, BeamSqlBackendCanSerialize)
import Database.Beam.Schema.Tables (Beamable, C, Table (PrimaryKey, primaryKey))
import Databases.HitmenBusiness.Handlers (HandlerT, PrimaryKey (HandlerId))
import Databases.HitmenBusiness.Util.Chronos (currentTimestamp_')
import Databases.HitmenBusiness.Util.JSON (flattenBase, noCamelOpt)
import Databases.HitmenBusiness.Util.Types (Codename)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (ToSample)
import Typeclass.Base (ToBase (..))
import Prelude (Maybe, Show, ($), (.))

data HitmanB f = Hitman
  { _codename :: C f Codename,
    _dieAt :: C (Nullable f) Datetime
  }
  deriving (Generic, Beamable)

data HitmanT f = HitmanAll
  { _hitmanId :: C f (SqlSerial Int32),
    _handlerId :: PrimaryKey HandlerT f,
    _base :: HitmanB f,
    _createdAt :: C f Datetime
  }
  deriving (Generic, Beamable)

type Hitman = HitmanB Identity

type HitmanAll = HitmanT Identity

instance ToJSON (HitmanB Identity) where
  toJSON = genericToJSON noCamelOpt
  toEncoding = genericToEncoding noCamelOpt

instance ToJSON (HitmanT Identity) where
  toJSON = flattenBase <$> genericToJSON noCamelOpt

instance FromHttpApiData HitmanId where
  parseUrlPiece = (HitmanId . SqlSerial <$>) . parseUrlPiece

instance ToHttpApiData HitmanId where
  toUrlPiece (HitmanId (SqlSerial i)) = toUrlPiece i

type HitmanId = PrimaryKey HitmanT Identity

deriving instance Show Hitman

deriving instance Show HitmanAll

deriving instance Show HitmanId

instance Table HitmanT where
  data PrimaryKey HitmanT f = HitmanId (C f (SqlSerial Int32)) deriving (Generic, Beamable)
  primaryKey = HitmanId . _hitmanId

instance FromJSON (HitmanB Identity) where
  parseJSON = genericParseJSON noCamelOpt

instance FromJSON (HitmanT Identity)

instance ToSample (SqlSerial Int32) => ToSample (PrimaryKey HitmanT Identity)

instance (ToSample (C f (Maybe Datetime)), ToSample (C f Codename)) => ToSample (HitmanB f)

instance
  ( ToSample (PrimaryKey HandlerT Identity),
    ToSample (SqlSerial Int32),
    ToSample Datetime
  ) =>
  ToSample (HitmanT Identity)
instance
  ( BeamSqlBackend be,
    BeamSqlBackendCanSerialize be Text,
    BeamSqlBackendCanSerialize be (Maybe Datetime)
  ) =>
  ToBase be HitmanT
  where
  type Base HitmanT = HitmanB
  fromBase b =
    HitmanAll
      { _hitmanId = default_,
        _base = val_ b,
        _createdAt = currentTimestamp_',
        -- _handlerId = val_ hid
        _handlerId = val_ (HandlerId 1) --  TEMP TEMP
      }
  baseAsUpdate body = (<-. val_ body) . _base
