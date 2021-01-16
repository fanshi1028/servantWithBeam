{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Utils.CRUD.UpdateRoute where

import Control.Natural (type (~>))
import Database.Beam (Database, DatabaseEntity, FromBackendRow, PrimaryKey, SqlUpdate, TableEntity, pk, update, val_, (==.))
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning (runUpdateReturningList))
import Servant (Capture, JSON, Put, ReqBody, (:>))
import Universum
import Utils.Constraints (UpdateBodyConstraint)
import Utils.Meta (WithMetaInfo (..), updateWithMetaInfo)

type UpdateApi a = (Capture "id" (PrimaryKey (WithMetaInfo a) Identity) :> ReqBody '[JSON] (a Identity) :> Put '[JSON] [WithMetaInfo a Identity])

updateOneSql ::
  UpdateBodyConstraint be a =>
  DatabaseEntity be db $ TableEntity $ WithMetaInfo a ->
  PrimaryKey (WithMetaInfo a) Identity ->
  a Identity ->
  SqlUpdate be $ WithMetaInfo a
updateOneSql table id body = update table (updateWithMetaInfo body) $ (==. val_ id) . pk

updateOne ::
  ( Database be db,
    UpdateBodyConstraint be a,
    FromBackendRow be (WithMetaInfo a Identity),
    MonadBeamUpdateReturning be m,
    Monad n
  ) =>
  (m ~> n) ->
  DatabaseEntity be db (TableEntity (WithMetaInfo a)) ->
  PrimaryKey (WithMetaInfo a) Identity ->
  a Identity ->
  n [WithMetaInfo a Identity]
updateOne doQuery table id = doQuery . runUpdateReturningList . updateOneSql table id
