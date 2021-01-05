{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module Utils.CRUD.DeleteRoute where

import Database.Beam (MonadBeam, PrimaryKey, SqlDelete, SqlValable (val_), delete, pk, runDelete, (==.))
import Database.Beam.Schema.Tables (DatabaseEntity, TableEntity)
import Servant (Capture, Delete, JSON, NoContent (NoContent), (:>))
import Universum
import Utils.Meta (WithMetaInfo)
import Utils.Constraints (DeleteOneConstraint)

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
  (forall t. m t -> n t) ->
  DatabaseEntity be db $ TableEntity $ WithMetaInfo a ->
  PrimaryKey (WithMetaInfo a) Identity ->
  n NoContent
deleteOne doQuery table = (>> return NoContent) . doQuery . runDelete . deleteOneSql table
