{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Utils.CRUD.CreateRoute where

import Control.Natural (type (~>))
import Database.Beam (MonadBeam, SqlInsert)
import Database.Beam.Query (insert, insertExpressions, runInsert)
import Database.Beam.Schema.Tables (DatabaseEntity, TableEntity)
import Servant (Handler, JSON, NoContent (NoContent), Post, ReqBody, (:>))
import Universum
import Utils.Constraints (CreateBodyConstraint)
import Utils.FromAccount (FromAccount (Base, fromAccount))
import Utils.Meta (Meta (..), WithMetaInfo)
import Utils.Types (MyServer, TableGetter)

type CreateApi a = ReqBody '[JSON] (a Identity) :> Post '[JSON] NoContent

createOneSql ::
  CreateBodyConstraint be a =>
  DatabaseEntity be db $ TableEntity $ WithMetaInfo a ->
  a Identity ->
  SqlInsert be $ WithMetaInfo a
createOneSql table body = insertExpressions [addMetaInfo body] & insert table

createOne ::
  (CreateBodyConstraint be a, MonadBeam be m) =>
  (m ~> MyServer be db conn msg Handler) ->
  TableGetter be db (WithMetaInfo a) ->
  a Identity ->
  MyServer be db conn msg Handler NoContent
createOne doQuery tableGet body = do
  table <- tableGet . view #_db <$> ask
  doQuery . runInsert $ createOneSql table body
  return NoContent

createOne' ::
  ( CreateBodyConstraint be a,
    FromAccount userInfo a,
    MonadBeam be m
  ) =>
  (m ~> MyServer be db conn msg Handler) ->
  TableGetter be db (WithMetaInfo a) ->
  WithMetaInfo userInfo Identity ->
  Base a Identity ->
  MyServer be db conn msg Handler NoContent
createOne' doQuery tableGet authInfo body = do
  table <- tableGet . view #_db <$> ask
  doQuery . runInsert . createOneSql table $ fromAccount authInfo body
  return NoContent
