{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Utils.CRUD.DeleteRoute where

import Database.Beam (HasSqlEqualityCheck, MonadBeam, PrimaryKey, SqlDelete, SqlValable (val_), delete, pk, runDelete, (==.))
import Database.Beam.Backend (BeamSqlBackendCanSerialize)
import Database.Beam.Query.Types (HasQBuilder)
import Database.Beam.Schema.Tables (DatabaseEntity, FieldsFulfillConstraint, Table, TableEntity)
import Servant (Capture, Delete, JSON, NoContent (NoContent), (:>))
import Universum
import Utils.Meta (WithMetaInfo)

type DeleteApi (a :: (* -> *) -> *) = Capture "id" (PrimaryKey (WithMetaInfo a) Identity) :> Delete '[JSON] NoContent

deleteOneSql ::
  ( HasQBuilder be,
    Table (WithMetaInfo a),
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) (PrimaryKey (WithMetaInfo a)),
    FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey (WithMetaInfo a))
  ) =>
  DatabaseEntity be db $ TableEntity $ WithMetaInfo a ->
  PrimaryKey (WithMetaInfo a) Identity ->
  SqlDelete be $ WithMetaInfo a
deleteOneSql table id = delete table $ (==. val_ id) . pk

deleteOne ::
  ( HasQBuilder be,
    Table (WithMetaInfo a),
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) (PrimaryKey (WithMetaInfo a)),
    FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey (WithMetaInfo a)),
    MonadBeam be m,
    Monad n
  ) =>
  (forall t. m t -> n t) ->
  DatabaseEntity be db $ TableEntity $ WithMetaInfo a ->
  PrimaryKey (WithMetaInfo a) Identity ->
  n NoContent
deleteOne doQuery table = (>> return NoContent) . doQuery . runDelete . deleteOneSql table
