{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Utils.CRUD.ReadRoute where

import Control.Monad.Except (MonadError)
import Database.Beam (Database, DatabaseEntity, MonadBeam, PrimaryKey, TableEntity, all_, lookup_, runSelectReturningList, runSelectReturningOne, select)
import Servant (Capture, Get, JSON, ServerError, err404, throwError, (:<|>), (:>))
import Universum
import Utils.Constraints (ReadAllConstraint, ReadOneConstraint)
import Utils.Meta (WithMetaInfo (..))

type ReadManyApi a = Get '[JSON] [WithMetaInfo a Identity]

type ReadOneApi a = Capture "id" (PrimaryKey (WithMetaInfo a) Identity) :> Get '[JSON] (WithMetaInfo a Identity)

type ReadApi a = ReadManyApi a :<|> ReadOneApi a

readOne ::
  ( Database be db,
    ReadOneConstraint be a,
    MonadBeam be m,
    MonadError ServerError n
  ) =>
  (forall t. m t -> n t) ->
  DatabaseEntity be db (TableEntity (WithMetaInfo a)) ->
  PrimaryKey (WithMetaInfo a) Identity ->
  n (WithMetaInfo a Identity)
readOne doQuery table id = lookup_ table id & runSelectReturningOne & doQuery >>= maybe (throwError err404) return

readMany ::
  ( ReadAllConstraint be a,
    Database be db,
    MonadBeam be m
  ) =>
  (forall t. m t -> n t) ->
  DatabaseEntity be db (TableEntity (WithMetaInfo a)) ->
  n [WithMetaInfo a Identity]
readMany doQuery table = doQuery $ runSelectReturningList $ select $ all_ table
