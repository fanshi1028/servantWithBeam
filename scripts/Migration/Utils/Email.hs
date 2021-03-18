{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Migraion.Databases.HitmenBusiness.Utils.Email where

import Database.Beam.AutoMigrate (ColumnType (SqlStdType), HasColumnType (..))
import Database.Beam.Backend (BeamBackend, FromBackendRow (..), HasSqlValueSyntax (..), varCharType)
import Universum

instance HasColumnType Email where
  defaultColumnType _ = SqlStdType $ varCharType Nothing Nothing
  defaultTypeCast _ = Just "character varying"
