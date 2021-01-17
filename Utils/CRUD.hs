{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Utils.CRUD
  ( simpleCRUDServer,
    SimpleCRUDAPI,
    simpleCRUDServerForHitmenBusiness,
    createOne,
    readOne,
    readMany,
    updateOne,
    deleteOne,
  )
where

import Colog (Message)
import Control.Natural (type (~>))
import Database.Beam (PrimaryKey)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning)
import Database.Beam.Postgres (Connection, Postgres)
import Database.Beam.Schema.Tables (Database, DatabaseEntity, TableEntity)
import GHC.TypeLits (Symbol)
import Lens.Micro (Getting)
import Servant (Capture, Handler, HasServer (ServerT), (:<|>) ((:<|>)), (:>))
import Servant.Docs (DocCapture (..), ToCapture (..))
import Universum
import Utils.CRUD.CreateRoute (CreateApi, createOne)
import Utils.CRUD.DeleteRoute (DeleteApi, deleteOne)
import Utils.CRUD.ReadRoute (ReadManyApi, ReadOneApi, readMany, readOne)
import Utils.CRUD.UpdateRoute (UpdateApi, updateOne)
import Utils.Constraints (CreateBodyConstraint, DeleteOneConstraint, ReadOneConstraint, UpdateBodyConstraint)
import Utils.Meta (WithMetaInfo)
import Utils.QueryRunner (doPgQueryWithDebug)
import Utils.Types (MyServer, TableGetter)

type SimpleCRUDAPI (path :: Symbol) a = path :> (CreateApi a :<|> ReadManyApi a :<|> ReadOneApi a :<|> UpdateApi a :<|> DeleteApi a)

instance ToCapture (Capture "id" $ PrimaryKey f Identity) where
  toCapture _ = DocCapture "id" "id"

simpleCRUDServer ::
  ( Database be db,
    CreateBodyConstraint be a,
    ReadOneConstraint be a,
    UpdateBodyConstraint be a,
    DeleteOneConstraint be a,
    MonadBeamUpdateReturning be m
  ) =>
  (m ~> MyServer be db conn msg Handler) ->
  TableGetter be db (WithMetaInfo a) ->
  ServerT (SimpleCRUDAPI path a) (MyServer be db conn msg Handler)
simpleCRUDServer doQuery tableGet =
  createOne doQuery tableGet
    :<|> readMany doQuery tableGet
    :<|> readOne doQuery tableGet
    :<|> updateOne doQuery tableGet
    :<|> deleteOne doQuery tableGet

simpleCRUDServerForHitmenBusiness ::
  ( Database Postgres db,
    CreateBodyConstraint Postgres a,
    ReadOneConstraint Postgres a,
    UpdateBodyConstraint Postgres a,
    DeleteOneConstraint Postgres a
  ) =>
  Getting
    (DatabaseEntity Postgres db $ TableEntity (WithMetaInfo a))
    (db (DatabaseEntity Postgres db))
    (DatabaseEntity Postgres db $ TableEntity (WithMetaInfo a)) ->
  ServerT
    (SimpleCRUDAPI path a)
    (MyServer Postgres db Connection Message Handler)
simpleCRUDServerForHitmenBusiness dbGetter = simpleCRUDServer doPgQueryWithDebug (view dbGetter)
