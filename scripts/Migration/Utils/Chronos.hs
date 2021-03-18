{-# LANGUAGE MultiParamTypeClasses #-}

module Migration.Utils.Chronos where

import Chronos (Datetime)
import Data.Time (LocalTime)
import Database.Beam.Backend (timestampType)
import Database.Beam.Migrate (HasDefaultSqlDataType (..))
import Database.Beam.AutoMigrate (HasColumnType (..))
import Universum
import Database.Beam.Postgres (Postgres)

instance HasDefaultSqlDataType Postgres Datetime where
  defaultSqlDataType _ _ _ = timestampType Nothing False

instance HasColumnType Datetime where
  defaultColumnType _ = defaultColumnType @LocalTime Proxy
  defaultTypeCast _ = defaultTypeCast @LocalTime Proxy
