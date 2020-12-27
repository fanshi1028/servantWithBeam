{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Utils.CRUD.CreateRoute where

import Database.Beam (MonadBeam)
import Database.Beam.Query (insert, insertExpressions, runInsert)
import Database.Beam.Query.Types (HasQBuilder)
import Database.Beam.Schema.Tables (DatabaseEntity, Table, TableEntity)
import Servant (JSON, NoContent (NoContent), Post, ReqBody, (:>))
import Typeclass.Meta (Meta (..), WithMetaInfo)
import Universum

class CreateRoute a where
  type CreateApi a :: *
  type CreateApi a = ReqBody '[JSON] (a Identity) :> Post '[JSON] NoContent
  createOne ::
    ( HasQBuilder be,
      Meta be a,
      Table (WithMetaInfo a),
      MonadBeam be m,
      Monad n
    ) =>
    (forall t. m t -> n t) ->
    DatabaseEntity be db (TableEntity (WithMetaInfo a)) ->
    a Identity ->
    n NoContent
  createOne doQuery table body = insertExpressions [addMetaInfo body] & insert table & runInsert & doQuery >> return NoContent
