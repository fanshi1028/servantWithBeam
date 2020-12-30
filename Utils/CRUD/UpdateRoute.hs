{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Utils.CRUD.UpdateRoute where

import Database.Beam (Beamable, Database, DatabaseEntity, HasQBuilder, HasSqlEqualityCheck, MonadBeam, PrimaryKey, Table, TableEntity, pk, runUpdate, update, val_, (==.))
import Database.Beam.Backend (BeamSqlBackendCanSerialize)
import Database.Beam.Schema.Tables (FieldsFulfillConstraint)
import Servant (Capture, JSON, NoContent (..), Put, ReqBody, (:>))
import Universum
import Utils.Meta (Meta, WithMetaInfo (..), updateWithMetaInfo)

type UpdateApi a = (Capture "id" (PrimaryKey (WithMetaInfo a) Identity) :> ReqBody '[JSON] (a Identity) :> Put '[JSON] NoContent)

updateOne ::
  ( HasQBuilder be,
    Database be db,
    With '[Beamable, Meta be] a,
    With '[Table] (WithMetaInfo a),
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) a,
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) (PrimaryKey (WithMetaInfo a)),
    FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey (WithMetaInfo a)),
    MonadBeam be m,
    With '[MonadIO] n
  ) =>
  (forall t. m t -> n t) ->
  DatabaseEntity be db (TableEntity (WithMetaInfo a)) ->
  PrimaryKey (WithMetaInfo a) Identity ->
  a Identity ->
  n NoContent
updateOne doQuery table id body = update table (updateWithMetaInfo body) ((==. val_ id) . pk) & runUpdate & doQuery >> return NoContent
