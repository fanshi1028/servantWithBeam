{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Utils.CRUD.ReadRoute where

import Control.Natural (type (~>))
import Database.Beam (Database, MonadBeam, PrimaryKey, all_, lookup_, runSelectReturningList, runSelectReturningOne, select)
import Servant (Capture, Get, Handler, JSON, err404, throwError, (:<|>), (:>))
import Universum
import Utils.Constraints (ReadAllConstraint, ReadOneConstraint)
import Utils.Meta (WithMetaInfo (..))
import Utils.Types (MyServer, TableGetter)

type ReadManyApi a = Get '[JSON] [WithMetaInfo a Identity]

type ReadOneApi a = Capture "id" (PrimaryKey (WithMetaInfo a) Identity) :> Get '[JSON] (WithMetaInfo a Identity)

type ReadApi a = ReadManyApi a :<|> ReadOneApi a

readOne ::
  (Database be db, ReadOneConstraint be a, MonadBeam be m) =>
  (m ~> MyServer be db conn msg Handler) ->
  TableGetter be db (WithMetaInfo a) ->
  PrimaryKey (WithMetaInfo a) Identity ->
  MyServer be db conn msg Handler (WithMetaInfo a Identity)
readOne doQuery tableGet id' = do
  ask >>= doQuery . runSelectReturningOne . flip lookup_ id' . tableGet . view #_db
    >>= maybe (throwError err404) return

readMany ::
  (ReadAllConstraint be a, Database be db, MonadBeam be m) =>
  (m ~> MyServer be db conn msg Handler) ->
  TableGetter be db (WithMetaInfo a) ->
  MyServer be db conn msg Handler [WithMetaInfo a Identity]
readMany doQuery tableGet = ask >>= doQuery . runSelectReturningList . select . all_ . tableGet . view #_db
