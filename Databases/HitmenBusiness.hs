{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}

module Databases.HitmenBusiness
  ( HitmenBusinessDb,
    hitmenBusinessDb,
    erasedMarkOf,
    markErasedBy,
    pursuingMarkOf,
    markPursuedBy,
    handlerIs,
    annotatedHitmenBusinessDb,
    hitmenBusinessDbSchema,
    ErasedMarkT,
    HandlerT,
    HitmanT,
    MarkT,
    PursuingMarkT,
  )
where

import Data.Char (isUpper)
import qualified Data.Char as C
import Data.Generics.Labels ()
import Database.Beam (dbModification)
import Database.Beam.AutoMigrate
  ( AnnotatedDatabaseSettings,
    UniqueConstraint (U),
    defaultAnnotatedDbSettings,
    fromAnnotatedDbSettings,
    uniqueConstraintOn,
  )
import Database.Beam.Query (oneToMany_, oneToOne_)
import Database.Beam.Schema (Database)
import Database.Beam.Schema.Tables (DatabaseSettings, TableEntity, defaultDbSettings, renamingFields, withDbModification)
import Databases.HitmenBusiness.ErasedMarks (ErasedMarkT (..))
import Databases.HitmenBusiness.Handlers (HandlerT (..))
import Databases.HitmenBusiness.Hitmen (HitmanT (..))
import Databases.HitmenBusiness.Marks (MarkT)
import Databases.HitmenBusiness.PursuingMarks (PursuingMarkT (..))

data HitmenBusinessDb f = HitmenBusinessDb
  { _hitmen :: f (TableEntity HitmanT),
    _handlers :: f (TableEntity HandlerT),
    _marks :: f (TableEntity MarkT),
    _hbPursuingMarks :: f (TableEntity PursuingMarkT),
    _hbErasedMarks :: f (TableEntity ErasedMarkT)
  }
  deriving (Generic, Database be)

hitmenBusinessDb :: DatabaseSettings be HitmenBusinessDb
hitmenBusinessDb =
  defaultDbSettings
    `withDbModification` renamingFields
      (toText . intercalate "_" . unCamelCase . dropWhile (== '_') . toString . last)
  where
    unCamelCase = \case
      "" -> []
      s -> case break isUpper s of
        (comp, []) -> [comp]
        (comp, next) ->
          let next' = maybe mempty (uncurry (:) . first C.toLower) (uncons next)
           in comp : unCamelCase next'

handlerIs handlers = oneToOne_ (hitmenBusinessDb ^. #_hitmen) (view #_handlerId) handlers

markPursuedBy hitmen = oneToMany_ (hitmenBusinessDb ^. #_hbPursuingMarks) (view $ #_base . #_hitmanId) hitmen

pursuingMarkOf marks = oneToOne_ (hitmenBusinessDb ^. #_hbPursuingMarks) (view $ #_base . #_markId) marks

markErasedBy hitmen = oneToOne_ (hitmenBusinessDb ^. #_hbErasedMarks) (view $ #_base . #_hitmanId) hitmen

erasedMarkOf marks = oneToOne_ (hitmenBusinessDb ^. #_hbErasedMarks) (view $ #_base . #_markId) marks

annotatedHitmenBusinessDb :: AnnotatedDatabaseSettings be HitmenBusinessDb
annotatedHitmenBusinessDb =
  defaultAnnotatedDbSettings hitmenBusinessDb
    `withDbModification` ( dbModification
                             { _handlers = uniqueConstraintOn [U . view $ #_base . #_codename],
                               _hitmen = uniqueConstraintOn [U . view $ #_base . #_codename],
                               _hbErasedMarks = uniqueConstraintOn [U . view $ #_base . #_markId],
                               _hbPursuingMarks = uniqueConstraintOn [U . view $ #_base . #_hitmanId, U $ view $ #_base . #_markId]
                             }
                             -- NOTE Why doesn't it work?
                             -- & #_handlers .~ uniqueConstraintOn [U (view $ #_base . #_codename)]
                         )

hitmenBusinessDbSchema = fromAnnotatedDbSettings annotatedHitmenBusinessDb (Proxy @'[])
