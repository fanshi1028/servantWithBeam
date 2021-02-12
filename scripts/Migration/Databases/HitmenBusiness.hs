{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Migration.Databases.HitmenBusiness where

import Database.Beam.AutoMigrate
  ( AnnotatedDatabaseSettings,
    Schema,
    UniqueConstraint (U),
    defaultAnnotatedDbSettings,
    fromAnnotatedDbSettings,
    uniqueConstraintOn,
  )

import Databases.HitmenBusiness (HitmenBusinessDb(..), hitmenBusinessDb)
import Migration.Databases.HitmenBusiness.Handlers ()
import Migration.Utils.Types ()
import Migration.Utils.Chronos ()
import Database.Beam (dbModification, withDbModification)
import Universum

annotatedHitmenBusinessDb :: AnnotatedDatabaseSettings be HitmenBusinessDb
annotatedHitmenBusinessDb =
  defaultAnnotatedDbSettings hitmenBusinessDb
    `withDbModification` ( dbModification
                             { _handlers = uniqueConstraintOn [U $ view $ #_base . #_codename],
                               _hitmen = uniqueConstraintOn [U $ view $ #_base . #_codename],
                               _hbErasedMarks = uniqueConstraintOn [U $ view $ #_base . #_markId],
                               _hbPursuingMarks = uniqueConstraintOn [U $ view $ #_base . #_hitmanId, U $ view $ #_base . #_markId],
                               _hbHandlersAccount = uniqueConstraintOn [U $ view #_accountName] <> uniqueConstraintOn [U $ view #_account]
                             }
                         )

hitmenBusinessDbSchema :: Schema
hitmenBusinessDbSchema = fromAnnotatedDbSettings annotatedHitmenBusinessDb (Proxy @'[])
