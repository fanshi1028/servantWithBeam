{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Migration.Utils.Types where

import Data.Password.Argon2 (Argon2, PasswordHash (..))
import Database.Beam.AutoMigrate (HasColumnType, PgEnum)
import Databases.HitmenBusiness.Utils.Types (Codename (..), FirstName (..), LastName (..), MarkDescription (..), MarkStatus (..))

-- | Codename
deriving newtype instance HasColumnType Codename

-- | FirstName
deriving newtype instance HasColumnType FirstName

-- | LastName
deriving newtype instance HasColumnType LastName

-- | MarkDescription
deriving newtype instance HasColumnType MarkDescription

-- | MarkStatus
deriving via (PgEnum MarkStatus) instance HasColumnType MarkStatus

-- | PasswordHash
deriving newtype instance HasColumnType (PasswordHash Argon2)
