{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Databases.HitmenBusiness.Util.Types (FirstName (..), LastName (..), Codename (..), MarkDescription (..), MarkStatus (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), decode)
import Data.Text (Text)
import Database.Beam (FromBackendRow (..), Generic, Typeable, val_)
import Database.Beam.AutoMigrate (HasColumnType, PgEnum)
import Database.Beam.Backend (BeamBackend, HasSqlValueSyntax (..), autoSqlValueSyntax)
import Text.Read (readMaybe)

-- | Codename
newtype Codename = Codename {unCodename :: Text}
  deriving newtype (ToJSON, Show, FromJSON, HasColumnType)
  deriving (Generic)

deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be Codename

deriving newtype instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be Codename

-- | FirstName
newtype FirstName = FirstName {unFirstName :: Text}
  deriving newtype (ToJSON, Show, FromJSON, HasColumnType)
  deriving (Generic)

deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be FirstName

deriving newtype instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be FirstName

-- | LastName
newtype LastName = LastName {unLastName :: Text}
  deriving newtype (ToJSON, Show, FromJSON, HasColumnType)
  deriving (Generic)

deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be LastName

deriving newtype instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be LastName

-- | MarkDescription
newtype MarkDescription = MarkDescription {unMarkDescription :: Text}
  deriving newtype (ToJSON, Show, FromJSON, HasColumnType)
  deriving (Generic)

deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be MarkDescription

deriving newtype instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be MarkDescription

-- | MarkStatus
data MarkStatus = Active | Erased | Cancelled
  deriving (Generic, FromJSON, Enum, Read, Show, Bounded, Typeable)
  deriving (HasColumnType) via (PgEnum MarkStatus)

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be MarkStatus where
  fromBackendRow = (readMaybe . show @Text <$> fromBackendRow) >>= maybe (fail "fail to get MarkStatus from backend row") return

instance HasSqlValueSyntax expr String => HasSqlValueSyntax expr MarkStatus where
  sqlValueSyntax = autoSqlValueSyntax

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

-- >>> toJSON $ Codename "hi"
-- String "hi"

-- >>> show $ Codename "hi"
-- "\"hi\""

-- instance BeamSqlBackendCanSerialize be Codename where
--   val_ cn = val_ $ unCodename cn
