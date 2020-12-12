{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Databases.HitmenBusiness.Handlers
  ( HandlerT (..),
    HandlerB (..),
    Handler,
    PrimaryKey (HandlerId),
    HandlerAll,
    HandlerId,
  )
where

-- import Control.Applicative (liftA2)
-- import Data.Aeson (withObject, (.:))
import Data.Aeson.Types
  ( FromJSON, -- (parseJSON)
    ToJSON,
  )
import Data.Generics.Labels ()
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time.LocalTime (LocalTime)
import Database.Beam (Generic, Identity, Nullable)
-- ($),

import Database.Beam.Backend (SqlSerial (SqlSerial, unSerial))
import Database.Beam.Backend.SQL (BeamSqlBackend, BeamSqlBackendCanSerialize)
import Database.Beam.Postgres (serial)
import Database.Beam.Query (SqlValable (val_), currentTimestamp_, default_, (<-.))
import Database.Beam.Schema.Tables (Beamable, C, Table (PrimaryKey, primaryKey))
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Typeclass.Base (ToBase (..))
import Prelude
  ( Maybe,
    Show,
    (.),
    (<$>),
  )

data HandlerB f = Handler
  { _codename :: C f Text,
    _dieAt :: C (Nullable f) LocalTime
  }
  deriving (Generic, Beamable)

data HandlerT f = HandlerAll
  { _id :: C f (SqlSerial Int32),
    _base :: HandlerB f,
    _createdAt :: C f LocalTime
  }
  deriving (Generic, Beamable)

type HandlerAll = HandlerT Identity

type Handler = HandlerB Identity

type HandlerId = PrimaryKey HandlerT Identity

deriving instance Show (HandlerB Identity)

instance FromHttpApiData HandlerId where
  parseUrlPiece = (HandlerId . SqlSerial <$>) . parseUrlPiece

instance ToHttpApiData HandlerId where
  toUrlPiece (HandlerId (SqlSerial i)) = toUrlPiece i

deriving instance ToJSON (HandlerB Identity)

deriving instance Show (HandlerT Identity)

deriving instance ToJSON (HandlerT Identity)

deriving instance Show (PrimaryKey HandlerT Identity)

instance ToJSON (PrimaryKey HandlerT Identity)

instance Table HandlerT where
  data PrimaryKey HandlerT f = HandlerId (C f (SqlSerial Int32)) deriving (Generic, Beamable)
  primaryKey = HandlerId . _id

-- instance FromJSON (HandlerB Identity) where
--   parseJSON = withObject "HandlerB" $ liftA2 Handler <$> (.: "codename") <*> (.: "dieAt")

instance FromJSON (HandlerB Identity)

instance FromJSON (HandlerT Identity)

instance FromJSON (PrimaryKey HandlerT Identity)

instance (BeamSqlBackend be, BeamSqlBackendCanSerialize be Text, BeamSqlBackendCanSerialize be (Maybe LocalTime)) => ToBase be HandlerT where
  type Base HandlerT = HandlerB

  -- type Extra HandlerT = ()
  -- fromBase b _ =
  fromBase b =
    HandlerAll
      { _id = default_,
        _base = val_ b,
        _createdAt = currentTimestamp_
      }
  baseAsUpdate body = (<-. val_ body) . _base

-- instance
--   ( BeamSqlBackend be,
--     BeamSqlBackendCanSerialize be Text,
--     BeamSqlBackendCanSerialize be UTCTime
--   ) =>
--   FromJSON (HandlerT (QExpr be s))
--   where
--   parseJSON = withObject "" $ \obj ->
--     HandlerAll
--       <$> (maybe default_ val_ <$> obj .:? "id")
--       <*> (Handler <$> (val_ <$> obj .: "codename") <*> (val_ <$> obj .: "dieAt"))
--       <*> (val_ <$> obj .: "createdAt")
