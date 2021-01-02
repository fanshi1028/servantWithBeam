{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Utils.CRUD.UpdateRoute where

import Database.Beam (Beamable, Database, DatabaseEntity, FromBackendRow, HasQBuilder, HasSqlEqualityCheck, PrimaryKey, SqlUpdate, Table, TableEntity, pk, update, val_, (==.))
import Database.Beam.Backend (BeamSqlBackendCanSerialize)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning (runUpdateReturningList))
import Database.Beam.Schema.Tables (FieldsFulfillConstraint)
import Servant (Capture, JSON, Put, ReqBody, (:>))
import Universum
import Utils.Meta (Meta, WithMetaInfo (..), updateWithMetaInfo)

type UpdateApi a = (Capture "id" (PrimaryKey (WithMetaInfo a) Identity) :> ReqBody '[JSON] (a Identity) :> Put '[JSON] [WithMetaInfo a Identity])

updateOneSql ::
  ( HasQBuilder be,
    Database be db,
    With '[Beamable, Meta be] a,
    With '[Table] (WithMetaInfo a),
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) a,
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) (PrimaryKey (WithMetaInfo a)),
    FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey (WithMetaInfo a))
  ) =>
  DatabaseEntity be db $ TableEntity $ WithMetaInfo a ->
  PrimaryKey (WithMetaInfo a) Identity ->
  a Identity ->
  SqlUpdate be $ WithMetaInfo a
updateOneSql table id body = update table (updateWithMetaInfo body) $ (==. val_ id) . pk

updateOne ::
  ( HasQBuilder be,
    Database be db,
    With '[Beamable, Meta be] a,
    With '[Table] (WithMetaInfo a),
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) a,
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) (PrimaryKey (WithMetaInfo a)),
    FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey (WithMetaInfo a)),
    FromBackendRow be (WithMetaInfo a Identity),
    MonadBeamUpdateReturning be m,
    With '[MonadIO] n
  ) =>
  (forall t. m t -> n t) ->
  DatabaseEntity be db (TableEntity (WithMetaInfo a)) ->
  PrimaryKey (WithMetaInfo a) Identity ->
  a Identity ->
  n [WithMetaInfo a Identity]
updateOne doQuery table id = doQuery . runUpdateReturningList . updateOneSql table id
