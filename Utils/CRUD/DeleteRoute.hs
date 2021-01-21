{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Utils.CRUD.DeleteRoute where

import Control.Natural (type (~>))
import Database.Beam (MonadBeam, PrimaryKey, SqlDelete, SqlValable (val_), delete, pk, runDelete, (==.))
import Database.Beam.Schema.Tables (DatabaseEntity, TableEntity)
import Servant (Capture, Delete, Handler, JSON, NoContent (NoContent), (:>))
import Universum
import Utils.Constraints (DeleteOneConstraint)
import Utils.Meta (WithMetaInfo)
import Utils.Types (MyServer, TableGetter)

type DeleteApi a = Capture "id" (PrimaryKey (WithMetaInfo a) Identity) :> Delete '[JSON] NoContent

deleteOneSql ::
  DeleteOneConstraint be a =>
  DatabaseEntity be db $ TableEntity $ WithMetaInfo a ->
  PrimaryKey (WithMetaInfo a) Identity ->
  SqlDelete be $ WithMetaInfo a
deleteOneSql table id' = delete table $ (==. val_ id') . pk

deleteOne ::
  (DeleteOneConstraint be a, MonadBeam be m) =>
  (m ~> MyServer be db conn msg Handler) ->
  TableGetter be db (WithMetaInfo a) ->
  PrimaryKey (WithMetaInfo a) Identity ->
  MyServer be db conn msg Handler NoContent
deleteOne doQuery tableGet id' =
  ask >>= doQuery . runDelete . flip deleteOneSql id' . tableGet . view #_db >> return NoContent
