{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Databases.HitmenBusiness.Utils.Types
  ( FirstName (..),
    LastName (..),
    Codename (..),
    MarkDescription (..),
    MarkStatus (..),
  )
where

import Control.Arrow (ArrowChoice ((|||)))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Generics.Labels ()
import Data.Password.Argon2 (Argon2, PasswordHash (..))
import Database.Beam (FromBackendRow (..), HasSqlEqualityCheck)
import Database.Beam.Backend (SqlSerial(..), BeamBackend, BeamSqlBackend, HasSqlValueSyntax (..))
import Servant.Auth.Server (SetCookie, def)
import Servant.Docs (ToSample (..), singleSample)
import Universum
import Data.Validity (declare, Validity(..))
import Data.Char (isPrint)

-- | Codename
newtype Codename = Codename {unCodename :: Text}
  deriving newtype (ToJSON, Show, FromJSON)
  deriving (Generic)

deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be Codename

deriving newtype instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be Codename

deriving newtype instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be Codename

instance ToSample Codename where
  toSamples _ = singleSample $ Codename "codename"

-- >>> validate (Codename "")
-- Validation {unValidation = [Violated "Must be non empty"]}
instance Validity Codename where
  validate (Codename name) =
    mconcat
      [ declare "Must be non empty" $ not (null name),
        declare "All Char must be printable" $ all isPrint name
      ]

-- | FirstName
newtype FirstName = FirstName {unFirstName :: Text}
  deriving newtype (ToJSON, Show, FromJSON)
  deriving Validity via Codename
  deriving (Generic)

deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be FirstName

deriving newtype instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be FirstName

instance ToSample FirstName where
  toSamples _ = singleSample $ FirstName "Pooh"

-- | LastName
newtype LastName = LastName {unLastName :: Text}
  deriving newtype (ToJSON, Show, FromJSON)
  deriving Validity via Codename
  deriving (Generic)

deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be LastName

deriving newtype instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be LastName

instance ToSample LastName where
  toSamples _ = singleSample $ LastName "Xi"

-- | MarkDescription
newtype MarkDescription = MarkDescription {unMarkDescription :: Text}
  deriving newtype (ToJSON, Show, FromJSON)
  deriving Validity via Codename
  deriving (Generic)

deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be MarkDescription

deriving newtype instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be MarkDescription

instance ToSample MarkDescription where
  toSamples _ = singleSample $ MarkDescription "He loves honey and killed millions for it"

-- | MarkStatus
data MarkStatus = Active | Erased | Cancelled
  deriving (Generic, FromJSON, Enum, Read, Show, Bounded, Typeable, ToSample, Validity)

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be MarkStatus where
  fromBackendRow = (readEither @Text @MarkStatus <$> fromBackendRow) >>= fail . toString ||| return

instance HasSqlValueSyntax expr Text => HasSqlValueSyntax expr MarkStatus where
  sqlValueSyntax = sqlValueSyntax . show @Text

-- | SetCookie
instance ToSample SetCookie where
  toSamples _ = singleSample def

-- | SqlSerial
deriving newtype instance Validity (SqlSerial Int32)
