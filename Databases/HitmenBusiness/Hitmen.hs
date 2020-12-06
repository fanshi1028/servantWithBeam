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
import Data.Aeson.Types (FromJSON (parseJSON), ToJSON, withObject, (.:))
import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam (Generic, Identity, Nullable, default_, val_, (<-.))
import Database.Beam.Backend (SqlSerial (SqlSerial))
import Database.Beam.Backend.SQL (BeamSqlBackend, BeamSqlBackendCanSerialize)
import Database.Beam.Schema.Tables (Beamable, C, Table (PrimaryKey, primaryKey))
import Databases.HitmenBusiness.Handlers (HandlerT, PrimaryKey (HandlerId))
import Databases.HitmenBusiness.Util.Chronos (currentTimestamp_')
import Databases.HitmenBusiness.Util.Types (Codename)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Typeclass.Base (ToBase (..))
import Prelude (Maybe, Show, ($), (.))

data HitmanB f = Hitman
  { _codename :: C f Codename,
    _dieAt :: C (Nullable f) Datetime
  }
  deriving (Generic, Beamable)

data HitmanT f = HitmanAll
  { _id :: C f (SqlSerial Int32),
    _handlerId :: PrimaryKey HandlerT f,
    _base :: HitmanB f,
    _createdAt :: C f Datetime
  }
  deriving (Generic, Beamable)

type Hitman = HitmanB Identity

deriving instance ToJSON (HitmanB Identity)

type HitmanAll = HitmanT Identity

deriving instance ToJSON (HitmanT Identity)

instance FromHttpApiData HitmanId where
  parseUrlPiece = (HitmanId . SqlSerial <$>) . parseUrlPiece

instance ToHttpApiData HitmanId where
  toUrlPiece (HitmanId (SqlSerial i)) = toUrlPiece i

type HitmanId = PrimaryKey HitmanT Identity

deriving instance Show Hitman

deriving instance Show HitmanAll

deriving instance Show HitmanId

-- deriving instance ToJSON (HitmanB Identity)

-- deriving instance ToJSON (HitmanT Identity)

-- deriving instance ToJSON (PrimaryKey HandlerT Identity)

instance Table HitmanT where
  data PrimaryKey HitmanT f = HitmanId (C f (SqlSerial Int32)) deriving (Generic, Beamable)
  primaryKey = HitmanId . _id

instance FromJSON (HitmanB Identity) where
  parseJSON = withObject "HitmanB" $ liftA2 Hitman <$> (.: "codename") <*> (.: "dieAt")

instance FromJSON (HitmanT Identity)

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
      { _id = default_,
        _base = val_ b,
        _createdAt = currentTimestamp_',
        -- _handlerId = val_ hid
        _handlerId = val_ (HandlerId 1) --  TEMP TEMP
      }
  baseAsUpdate body = (<-. val_ body) . _base
