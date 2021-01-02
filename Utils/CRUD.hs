{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Utils.CRUD
  ( simpleCRUDServer,
    SimpleCRUDAPI,
    simpleCRUDServerForHitmenBusiness,
    -- simpleCRUDServerForHitmenBusinessLite,
    createOne,
    readOne,
    readMany,
    updateOne,
    deleteOne,
  )
where

import Control.Monad.Except (MonadError)
import Database.Beam (FromBackendRow, MonadBeam, PrimaryKey)
import Database.Beam.Backend.SQL (BeamSqlBackendCanSerialize)
import Database.Beam.Query (HasSqlEqualityCheck)
import Database.Beam.Query.Types (HasQBuilder)
import Database.Beam.Schema.Tables (Beamable, Database, DatabaseEntity, FieldsFulfillConstraint, Table, TableEntity)
import Databases.HitmenBusiness (hitmenBusinessDb)
import GHC.TypeLits (Symbol)
import Servant (Capture, Delete, Get, HasServer (ServerT), JSON, NoContent, Post, Put, ReqBody, ServerError, (:<|>) ((:<|>)), (:>))
import Servant.Docs (HasDocs, DocCapture (..), ToCapture (..))
import Universum
import Utils.CRUD.CreateRoute (CreateApi, createOne, createOneSql)
import Utils.CRUD.DeleteRoute (DeleteApi, deleteOne)
import Utils.CRUD.ReadRoute (ReadApi, ReadManyApi, ReadOneApi, readMany, readOne)
import Utils.CRUD.UpdateRoute (UpdateApi, updateOne)
import Utils.Meta (Meta (..), WithMetaInfo)
import Utils.QueryRunner (doPgQueryWithDebug, doSqliteQueryWithDebug)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning)

type SimpleCRUDAPI (path :: Symbol) a = path :> (CreateApi a :<|> ReadManyApi a :<|> ReadOneApi a :<|> UpdateApi a :<|> DeleteApi a)

instance ToCapture (Capture "id" $ PrimaryKey f Identity) where
  toCapture _ = DocCapture "id" "id"

simpleCRUDServer ::
  ( HasQBuilder be,
    Database be db,
    With '[Beamable, Meta be] a,
    Table (WithMetaInfo a),
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) a,
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) (WithMetaInfo a),
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) (PrimaryKey (WithMetaInfo a)),
    FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey (WithMetaInfo a)),
    FromBackendRow be (WithMetaInfo a Identity),
    MonadBeamUpdateReturning be m,
    With '[MonadIO, MonadError ServerError] n
  ) =>
  (forall t. m t -> n t) ->
  DatabaseEntity be db (TableEntity (WithMetaInfo a)) ->
  ServerT (SimpleCRUDAPI path a) n
simpleCRUDServer doQuery table =
  createOne doQuery table
    :<|> readMany doQuery table
    :<|> readOne doQuery table
    :<|> updateOne doQuery table
    :<|> deleteOne doQuery table

simpleCRUDServerForHitmenBusiness dbGetter = simpleCRUDServer doPgQueryWithDebug (hitmenBusinessDb ^. dbGetter)

-- simpleCRUDServerForHitmenBusinessLite dbGetter = simpleCRUDServer doSqliteQueryWithDebug (hitmenBusinessDb ^. dbGetter)
