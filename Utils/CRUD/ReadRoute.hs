{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Utils.CRUD.ReadRoute where

import Control.Monad.Except (MonadError)
import Database.Beam (Database, DatabaseEntity, FromBackendRow, HasQBuilder, HasSqlEqualityCheck, MonadBeam, PrimaryKey, Table, TableEntity, all_, lookup_, runSelectReturningList, runSelectReturningOne, select)
import Database.Beam.Backend (BeamSqlBackendCanSerialize)
import Database.Beam.Schema.Tables (FieldsFulfillConstraint)
import Servant (Capture, Get, JSON, ServerError, err404, throwError, (:<|>), (:>))
import Typeclass.Meta (WithMetaInfo (..))
import Universum

class ReadRoute a where
  type ReadApi a :: *
  type
    ReadApi a =
      Get '[JSON] [WithMetaInfo a Identity]
        :<|> (Capture "id" (PrimaryKey (WithMetaInfo a) Identity) :> Get '[JSON] (WithMetaInfo a Identity))
  readOne ::
    ( HasQBuilder be,
      Database be db,
      With '[Table] (WithMetaInfo a),
      FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) (PrimaryKey (WithMetaInfo a)),
      FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey (WithMetaInfo a)),
      FromBackendRow be (WithMetaInfo a Identity),
      MonadBeam be m,
      MonadError ServerError n
    ) =>
    (forall t. m t -> n t) ->
    DatabaseEntity be db (TableEntity (WithMetaInfo a)) ->
    PrimaryKey (WithMetaInfo a) Identity ->
    n (WithMetaInfo a Identity)
  readOne doQuery table id = lookup_ table id & runSelectReturningOne & doQuery >>= maybe (throwError err404) return
  readMany ::
    ( HasQBuilder be,
      Database be db,
      With '[Table] (WithMetaInfo a),
      FromBackendRow be (WithMetaInfo a Identity),
      MonadBeam be m
    ) =>
    (forall t. m t -> n t) ->
    DatabaseEntity be db (TableEntity (WithMetaInfo a)) ->
    n [WithMetaInfo a Identity]
  readMany doQuery table = doQuery $ runSelectReturningList $ select $ all_ table
