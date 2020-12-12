{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Models.HitmenBusiness
  ( getAllHitmen,
    getAllHitmenPursuingActiveMarks,
    getAllErasedMarksSinceBy,
    getAllErasedMarksSince,
    getAllPursuingMarks,
    getAllMarks,
    getAllHandlers,
    getActiveMarkWithSinglePurserFrom,
    getActiveMarkWithSinglePurser,
    getMarkOfOpportunityIn,
    getMarkOfOpportunity,
    getLatestKillsBy,
    getLatestKills,
    getBountiesAwardedTo,
    getBountiesAwarded,
  )
where

-- import Chronos.Types (Time)

import Data.Int (Int32)
import Data.Time.LocalTime (LocalTime)
import Database.Beam (filter_', pk)
import Database.Beam.Backend.SQL (BeamSqlBackend)
import Database.Beam.Query (HasSqlEqualityCheck, HasSqlQuantifiedEqualityCheck, QGroupable (group_), QValueContext, SqlDeconstructMaybe (maybe_), SqlJustable (just_), SqlValable (val_), aggregate_, allOf_, all_, count_, filter_, guard_, join_, leftJoin_, max_, sum_, (/=*.), (==.), (>.), (>=.))
import Database.Beam.Query.Internal (QNested)
import Database.Beam.Query.Types (HasQBuilder, Q, QExpr, QGenExpr)
import Databases.HitmenBusiness
  ( HitmenBusinessDb,
    erasedMarkOf,
    hitmenBusinessDb,
    markErasedBy,
    markPursuedBy,
  )
import Databases.HitmenBusiness.ErasedMarks (ErasedMarkT)
import Databases.HitmenBusiness.Handlers (HandlerT)
import Databases.HitmenBusiness.Hitmen (HitmanT)
import Databases.HitmenBusiness.Marks (MarkT, PrimaryKey (MarkId))
import Databases.HitmenBusiness.PursuingMarks (PursuingMarkT)
import Lens.Micro ((&), (^.))
import Lens.Micro.Extras (view)

-- NOTE util

myJoin f x = do
  y <- f x
  return (x, y)

-- NOTE basic

-- Get all handlers
getAllHandlers :: BeamSqlBackend be => Q be HitmenBusinessDb s (HandlerT (QExpr be s))
getAllHandlers = all_ $ hitmenBusinessDb ^. #_handlers

-- Get all the hitmen
getAllHitmen :: BeamSqlBackend be => Q be HitmenBusinessDb s (HitmanT (QExpr be s))
getAllHitmen = all_ $ hitmenBusinessDb ^. #_hitmen

-- Get all erased marks
getAllMarks :: BeamSqlBackend be => Q be HitmenBusinessDb s (MarkT (QExpr be s))
getAllMarks = all_ $ hitmenBusinessDb ^. #_marks

-- Get all erased marks
getAllErasedMarks :: BeamSqlBackend be => Q be HitmenBusinessDb s (ErasedMarkT (QExpr be s))
getAllErasedMarks = all_ $ hitmenBusinessDb ^. #_hbErasedMarks

-- Get all erased marks
getAllPursuingMarks :: BeamSqlBackend be => Q be HitmenBusinessDb s (PursuingMarkT (QExpr be s))
getAllPursuingMarks = all_ $ hitmenBusinessDb ^. #_hbPursuingMarks

-- Check if mark is active
-- isActiveMarks :: (BeamSqlBackend be, HasSqlQuantifiedEqualityCheck be Int32, HasQBuilder be, BeamSqlBackendCanSerialize be Int32) => PursuingMarkT (QExpr be s) -> QGenExpr QValueContext be s SqlBool

-- isActiveMarks :: (BeamSqlBackend be, HasQBuilder be) => PursuingMarkT (QExpr be s) -> QGenExpr QValueContext be s SqlBool
-- isActiveMarks m =
-- (m, _) <- mark >>= myJoin pursuingMarkOf
-- return $ pk m /=*. allOf_ (val_ . view #_erasedmarkMarkId <$> getAllErasedMarks)
-- anyIn_ $ fst <$> getAllMarks >>= myJoin erasedMarkOf

-- view #_pursuingmarkMarkId m /=*. allOf_ (pk m)
-- (view #_pursuingmarkMarkId m) /=*.

-- allOf_ $ view #_erasedmarkMarkId <$> getAllErasedMarks

-- return allOf_ (view #_erasedmarkMarkId <$> getAllErasedMarks)

-- get active marks by hitmen
-- getActiveMarks hitmen = filter_' isActiveMarks $ markPursuedBy hitmen
-- getActiveMarks :: (BeamSqlBackend be, HasQBuilder be, HasSqlQuantifiedEqualityCheck be Int32) => HitmanT (QExpr be s) -> Q be HitmenBusinessDb s (PursuingMarkT (QExpr be s))
-- getActiveMarks :: _
getActiveMarks :: (BeamSqlBackend be, HasQBuilder be, HasSqlQuantifiedEqualityCheck be Int32) => HitmanT (QExpr be s) -> Q be HitmenBusinessDb s (PursuingMarkT (QExpr be s))
getActiveMarks hitmen =
  let unwrapId (MarkId mid) = mid
      -- erasedIds :: (BeamSqlBackend be) => Q be HitmenBusinessDb s (QGenExpr QValueContext be s Int32)
      erasedIds = unwrapId . view (#_base . #_markId) <$> getAllErasedMarks
   in filter_' ((/=*. allOf_ erasedIds) . unwrapId . view (#_base . #_markId)) $ markPursuedBy hitmen

-- getActiveMarks hitmen = filter_' _ $ markPursuedBy hitmen

-- getActiveMarks = filter_' isActiveMarks . markPursuedBy

-- Get all the hitmen that are pursuing active marks (i.e. marks that haven’t been erased yet)
getAllHitmenPursuingActiveMarks ::
  (BeamSqlBackend be, HasSqlQuantifiedEqualityCheck be Int32, HasQBuilder be) => Q be HitmenBusinessDb s (HitmanT (QExpr be s), PursuingMarkT (QExpr be s))
getAllHitmenPursuingActiveMarks = getAllHitmen >>= myJoin getActiveMarks

--  Get all the marks that have been erased since a given date by a given hitman
getAllErasedMarksSinceBy date hitmen = filter_ ((>=. date) <$> view #_createdAt) $ markErasedBy hitmen

-- Get all the marks that have been erased since a given date
getAllErasedMarksSince date = getAllHitmen >>= getAllErasedMarksSinceBy date

-- Get all the active marks that have only a single pursuer
getActiveMarkWithSinglePurserFrom hitmen = do
  myJoin getActiveMarks hitmen
    & aggregate_ (\(h, pm) -> (group_ pm, count_ $ h ^. #_id))
    & filter_ (\(_, count) -> count ==. 1)

getActiveMarkWithSinglePurser ::
  (HasQBuilder be, HasSqlQuantifiedEqualityCheck be Int32) =>
  Q
    be
    HitmenBusinessDb
    (QNested s)
    (Q be HitmenBusinessDb s (PursuingMarkT (QGenExpr QValueContext be s), QGenExpr QValueContext be s Int32))
getActiveMarkWithSinglePurser = getActiveMarkWithSinglePurserFrom <$> getAllHitmen

-- Marks of opportunity (i.e. marks that a hitman erased without them marking the mark as being pursued first)
getMarkOfOpportunityIn marks = do
  (m, em) <- myJoin erasedMarkOf marks
  mpm <- leftJoin_ getAllPursuingMarks $ (==. pk m) <$> view (#_base . #_markId)
  guard_ $ maybe_ (val_ True) ((>. em ^. #_createdAt) <$> view #_createdAt) mpm
  return (m, em)

getMarkOfOpportunity ::
  (BeamSqlBackend be, HasSqlEqualityCheck be Int32) =>
  Q be HitmenBusinessDb s (MarkT (QExpr be s), ErasedMarkT (QExpr be s))
getMarkOfOpportunity = getAllMarks >>= getMarkOfOpportunityIn

-- Latest kill by specific hitmen
getLatestKillsBy hitmen = do
  (h, mlatest) <- aggregate_ (\(h, em) -> (group_ h, max_ $ em ^. #_createdAt)) $ myJoin markErasedBy hitmen
  filter_ ((==. mlatest) . just_ <$> (view #_createdAt . snd)) $ myJoin markErasedBy h

-- Latest kills
getLatestKills ::
  (BeamSqlBackend be, HasSqlEqualityCheck be Int32, HasSqlEqualityCheck be LocalTime) =>
  Q be HitmenBusinessDb (QNested s) (Q be HitmenBusinessDb s (HitmanT (QExpr be s), ErasedMarkT (QExpr be s)))
getLatestKills = getLatestKillsBy <$> getAllHitmen

-- Get the total bounty awarded to a specific hitman
getBountiesAwardedTo hitmen =
  aggregate_ (\(h, m) -> (group_ h, sum_ $ m ^. #_base . #_listBounty)) $
    do
      (h, em) <- myJoin markErasedBy hitmen
      m <- join_ (hitmenBusinessDb ^. #_marks) $ (==. em ^. #_base . #_markId) <$> pk
      return (h, m)

-- Get the total bounty awarded to each hitman
getBountiesAwarded ::
  (BeamSqlBackend be, HasSqlEqualityCheck be Int32) =>
  Q
    be
    HitmenBusinessDb
    (QNested s)
    (Q be HitmenBusinessDb s (HitmanT (QGenExpr QValueContext be s), QGenExpr QValueContext be s (Maybe Int32)))
getBountiesAwarded = getBountiesAwardedTo <$> getAllHitmen
