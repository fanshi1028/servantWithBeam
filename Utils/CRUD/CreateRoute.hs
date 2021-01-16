{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Utils.CRUD.CreateRoute where

import Control.Natural (type (~>))
import Database.Beam (MonadBeam, SqlInsert)
import Database.Beam.Query (insert, insertExpressions, runInsert)
import Database.Beam.Schema.Tables (DatabaseEntity, TableEntity)
import Servant (JSON, NoContent (NoContent), Post, ReqBody, (:>))
import Universum
import Utils.Constraints (CreateBodyConstraint)
import Utils.FromAccount (FromAccount (Base, fromAccount))
import Utils.Meta (Meta (..), WithMetaInfo)

type CreateApi a = ReqBody '[JSON] (a Identity) :> Post '[JSON] NoContent

createOneSql ::
  CreateBodyConstraint be a =>
  DatabaseEntity be db $ TableEntity $ WithMetaInfo a ->
  a Identity ->
  SqlInsert be $ WithMetaInfo a
createOneSql table body = insertExpressions [addMetaInfo body] & insert table

createOne ::
  ( CreateBodyConstraint be a,
    MonadBeam be m,
    Monad n
  ) =>
  (m ~> n) ->
  DatabaseEntity be db $ TableEntity $ WithMetaInfo a ->
  a Identity ->
  n NoContent
createOne doQuery table = (>> return NoContent) . doQuery . runInsert . createOneSql table

createOne' ::
  ( CreateBodyConstraint be a,
    FromAccount userInfo a,
    MonadBeam be m,
    Monad n
  ) =>
  (m ~> n) ->
  DatabaseEntity be db $ TableEntity $ WithMetaInfo a ->
  WithMetaInfo userInfo Identity ->
  Base a Identity ->
  n NoContent
createOne' doQuery table authInfo = (>> return NoContent) . doQuery . runInsert . createOneSql table . fromAccount authInfo
