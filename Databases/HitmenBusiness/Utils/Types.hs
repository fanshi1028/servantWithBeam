{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Databases.HitmenBusiness.Utils.Types (FirstName (..), LastName (..), Codename (..), MarkDescription (..), MarkStatus (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Database.Beam (FromBackendRow (..))
import Database.Beam.AutoMigrate (HasColumnType, PgEnum)
import Database.Beam.Backend (BeamBackend, HasSqlValueSyntax (..))
import Servant.Docs (ToSample (..), singleSample)
import Universum

-- | Codename
newtype Codename = Codename {unCodename :: Text}
  deriving newtype (ToJSON, Show, FromJSON, HasColumnType)
  deriving (Generic)

deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be Codename

deriving newtype instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be Codename

instance ToSample Codename where
  toSamples _ = singleSample $ Codename "codename"

-- | FirstName
newtype FirstName = FirstName {unFirstName :: Text}
  deriving newtype (ToJSON, Show, FromJSON, HasColumnType)
  deriving (Generic)

deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be FirstName

deriving newtype instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be FirstName

instance ToSample FirstName where
  toSamples _ = singleSample $ FirstName "Pooh"

-- | LastName
newtype LastName = LastName {unLastName :: Text}
  deriving newtype (ToJSON, Show, FromJSON, HasColumnType)
  deriving (Generic)

deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be LastName

deriving newtype instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be LastName

instance ToSample LastName where
  toSamples _ = singleSample $ LastName "Xi"

-- | MarkDescription
newtype MarkDescription = MarkDescription {unMarkDescription :: Text}
  deriving newtype (ToJSON, Show, FromJSON, HasColumnType)
  deriving (Generic)

deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be MarkDescription

deriving newtype instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be MarkDescription

instance ToSample MarkDescription where
  toSamples _ = singleSample $ MarkDescription "He loves honey and killed millions for it"

-- | MarkStatus
data MarkStatus = Active | Erased | Cancelled
  deriving (Generic, FromJSON, Enum, Read, Show, Bounded, Typeable, ToSample)
  deriving (HasColumnType) via (PgEnum MarkStatus)

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be MarkStatus where
  fromBackendRow = (readEither @Text @MarkStatus <$> fromBackendRow) >>= either (fail . toString) return

instance HasSqlValueSyntax expr Text => HasSqlValueSyntax expr MarkStatus where
  sqlValueSyntax = sqlValueSyntax . show @Text