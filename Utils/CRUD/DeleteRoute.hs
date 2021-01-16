{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Utils.CRUD.DeleteRoute where

import Control.Natural (type (~>))
import Database.Beam (MonadBeam, PrimaryKey, SqlDelete, SqlValable (val_), delete, pk, runDelete, (==.))
import Database.Beam.Schema.Tables (DatabaseEntity, TableEntity)
import Servant (Capture, Delete, JSON, NoContent (NoContent), (:>))
import Universum
import Utils.Constraints (DeleteOneConstraint)
import Utils.Meta (WithMetaInfo)

type DeleteApi a = Capture "id" (PrimaryKey (WithMetaInfo a) Identity) :> Delete '[JSON] NoContent

deleteOneSql ::
  DeleteOneConstraint be a =>
  DatabaseEntity be db $ TableEntity $ WithMetaInfo a ->
  PrimaryKey (WithMetaInfo a) Identity ->
  SqlDelete be $ WithMetaInfo a
deleteOneSql table id = delete table $ (==. val_ id) . pk

deleteOne ::
  ( DeleteOneConstraint be a,
    MonadBeam be m,
    Monad n
  ) =>
  (m ~> n) ->
  DatabaseEntity be db $ TableEntity $ WithMetaInfo a ->
  PrimaryKey (WithMetaInfo a) Identity ->
  n NoContent
deleteOne doQuery table = (>> return NoContent) . doQuery . runDelete . deleteOneSql table
