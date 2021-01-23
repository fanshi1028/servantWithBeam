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
    ErasedMarkB,
    HandlerB,
    HitmanB,
    MarkB,
    PursuingMarkB,
    ErasedMarkT,
    HandlerT,
    HitmanT,
    MarkT,
    PursuingMarkT,
    acountOf,
  )
where

import Data.Char (isUpper)
import qualified Data.Char as C
import Data.Generics.Labels ()
import Data.Password.Argon2 (Argon2)
import Database.Beam (HasSqlEqualityCheck, Q, QExpr, dbModification)
import Database.Beam.AutoMigrate
  ( AnnotatedDatabaseSettings,
    Schema,
    UniqueConstraint (U),
    defaultAnnotatedDbSettings,
    fromAnnotatedDbSettings,
    uniqueConstraintOn,
  )
import Database.Beam.Query (oneToMany_, oneToOne_)
import Database.Beam.Schema (Database)
import Database.Beam.Schema.Tables (DatabaseSettings, TableEntity, defaultDbSettings, renamingFields, withDbModification)
import Databases.HitmenBusiness.ErasedMarks (ErasedMarkB, ErasedMarkT)
import Databases.HitmenBusiness.Handlers (HandlerB, HandlerT)
import Databases.HitmenBusiness.Hitmen (HitmanB, HitmanT)
import Databases.HitmenBusiness.Marks (MarkB, MarkT)
import Databases.HitmenBusiness.PursuingMarks (PursuingMarkB, PursuingMarkT)
import Universum
import Utils.Account.Login (LoginT)
import Utils.Meta (WithMetaInfo)

data HitmenBusinessDb f = HitmenBusinessDb
  { _hitmen :: f (TableEntity HitmanT),
    _handlers :: f (TableEntity HandlerT),
    _marks :: f (TableEntity MarkT),
    _hbPursuingMarks :: f (TableEntity PursuingMarkT),
    _hbErasedMarks :: f (TableEntity ErasedMarkT),
    _hbHandlersAccount :: f (TableEntity $ LoginT Argon2 HandlerB)
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

handlerIs :: HasSqlEqualityCheck be Int32 => HandlerT (QExpr be s) -> Q be HitmenBusinessDb s (HitmanT (QExpr be s))
handlerIs = oneToOne_ (hitmenBusinessDb ^. #_hitmen) (view $ #_base . #_handlerId)

markPursuedBy :: HasSqlEqualityCheck be Int32 => HitmanT (QExpr be s) -> Q be HitmenBusinessDb s (PursuingMarkT (QExpr be s))
markPursuedBy = oneToMany_ (hitmenBusinessDb ^. #_hbPursuingMarks) (view $ #_base . #_hitmanId)

pursuingMarkOf :: HasSqlEqualityCheck be Int32 => MarkT (QExpr be s) -> Q be HitmenBusinessDb s (PursuingMarkT (QExpr be s))
pursuingMarkOf = oneToOne_ (hitmenBusinessDb ^. #_hbPursuingMarks) (view $ #_base . #_markId)

markErasedBy :: HasSqlEqualityCheck be Int32 => HitmanT (QExpr be s) -> Q be HitmenBusinessDb s (ErasedMarkT (QExpr be s))
markErasedBy = oneToOne_ (hitmenBusinessDb ^. #_hbErasedMarks) (view $ #_base . #_hitmanId)

erasedMarkOf :: HasSqlEqualityCheck be Int32 => MarkT (QExpr be s) -> Q be HitmenBusinessDb s (ErasedMarkT (QExpr be s))
erasedMarkOf = oneToOne_ (hitmenBusinessDb ^. #_hbErasedMarks) (view $ #_base . #_markId)

acountOf :: HasSqlEqualityCheck be Int32 => WithMetaInfo HandlerB (QExpr be s) -> Q be HitmenBusinessDb s (LoginT Argon2 HandlerB (QExpr be s))
acountOf = oneToOne_ (hitmenBusinessDb ^. #_hbHandlersAccount) (view #_account)

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
