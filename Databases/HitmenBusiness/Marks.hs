{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Applicative (Applicative ((<*>)), (<$>))
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON (parseJSON), withObject, (.:))
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time.LocalTime (LocalTime)
import Database.Beam (Generic, Identity, Nullable, Typeable)
import Database.Beam.AutoMigrate (HasColumnType (..), PgEnum)
import Database.Beam.Backend (SqlSerial (SqlSerial))
import Database.Beam.Backend.SQL (BeamSqlBackend, BeamSqlBackendCanSerialize, HasSqlValueSyntax (..), autoSqlValueSyntax)
import Database.Beam.Backend.SQL.Row (FromBackendRow (..))
import Database.Beam.Backend.Types (BeamBackend)
import Database.Beam.Query (SqlValable (val_), currentTimestamp_, default_, (<-.))
import Database.Beam.Schema.Tables (Beamable, C, Table (PrimaryKey, primaryKey))
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Text.Read (Read (..), readMaybe)
import Text.Show (Show (..))
import Typeclass.Base (ToBase (..))
import Prelude (Bounded, Enum, Maybe (..), String, fail, maybe, return, ($), (.), (>>=))

instance FromJSON (PrimaryKey MarkT Identity)

-- data MarkStatus = Active | Erased | ReplacedBy MarkId | Cancelled deriving (Generic, FromJSON)
data MarkStatus = Active | Erased | Cancelled
  deriving (Generic, FromJSON, Enum, Read, Show, Bounded, Typeable)
  deriving (HasColumnType) via (PgEnum MarkStatus)

-- instance Read MarkStatus where
--   readPrec =
--     parens $
--       prec 10 $
--         lexP
--           >>= ( \case
--                   (Ident "active") -> return Active
--                   (Ident "erased") -> return Erased
--                   (Ident "cancelled") -> return Cancelled
--                   (Number n) -> maybe pfail (return . ReplacedBy . MarkId . fromIntegral) $ numberToInteger n
--                   _ -> pfail
--               )

-- instance Show MarkStatus where
--   showsPrec d = \case
--     Active -> showParen (d > app_prec) $ showString "active"
--     Erased -> showParen (d > app_prec) $ showString "erased"
--     ReplacedBy (MarkId i) -> showsPrec (app_prec + 1) i
--     Cancelled -> showParen (d > app_prec) $ showString "cancelled"
--     where
--       app_prec = 10

instance HasSqlValueSyntax expr String => HasSqlValueSyntax expr MarkStatus where
  sqlValueSyntax = autoSqlValueSyntax

data MarkB f = Mark
  { _listBounty :: C f Int32,
    _firstName :: C f Text,
    _lastName :: C f Text,
    _description :: C (Nullable f) Text,
    _status :: C f MarkStatus
  }
  deriving (Generic, Beamable)

data MarkT f = MarkAll
  { _mid :: C f (SqlSerial Int32),
    _createdAt :: C f LocalTime,
    _updatedAt :: C f LocalTime,
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

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be MarkStatus where
  fromBackendRow = (readMaybe . show @Text <$> fromBackendRow) >>= maybe (fail "fail to get MarkStatus from backend row") return

instance Table MarkT where
  data PrimaryKey MarkT f = MarkId (C f (SqlSerial Int32)) deriving (Generic, Beamable)
  primaryKey = MarkId . _mid

instance FromJSON (MarkB Identity) where
  parseJSON = withObject "MarkB" $ \obj ->
    Mark <$> (obj .: "list_bounty") <*> (obj .: "first_name") <*> (obj .: "last_name") <*> (obj .: "description") <*> (obj .: "status")

instance FromJSON (MarkT Identity)

instance
  ( BeamSqlBackend be,
    BeamSqlBackendCanSerialize be (Maybe Text),
    BeamSqlBackendCanSerialize be Text,
    BeamSqlBackendCanSerialize be Int32,
    BeamSqlBackendCanSerialize be LocalTime,
    BeamSqlBackendCanSerialize be MarkStatus
  ) =>
  ToBase be MarkT
  where
  type Base MarkT = MarkB

  -- type Extra MarkT = ()
  -- fromBase b _ =
  fromBase b =
    MarkAll
      { _mid = default_,
        _base = val_ b,
        _createdAt = currentTimestamp_,
        _updatedAt = currentTimestamp_
      }
  baseAsUpdate body = (<-. val_ body) . _base
