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
    simpleCRUDServerForHitmenBusinessLite,
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
import Servant.Docs (DocCapture (..), ToCapture (..))
import Universum
import Utils.CRUD.CreateRoute (createOne)
import Utils.CRUD.DeleteRoute (deleteOne)
import Utils.CRUD.ReadRoute (readMany, readOne)
import Utils.CRUD.UpdateRoute (updateOne)
import Utils.Meta (Meta (..), WithMetaInfo)
import Utils.QueryRunner (doPgQueryWithDebug, doSqliteQueryWithDebug)

type SimpleCRUDAPI (path :: Symbol) a =
  path
    :> ( (ReqBody '[JSON] (a Identity) :> Post '[JSON] NoContent)
           :<|> Get '[JSON] [WithMetaInfo a Identity]
           :<|> (Capture "id" (PrimaryKey (WithMetaInfo a) Identity) :> Get '[JSON] (WithMetaInfo a Identity))
           :<|> (Capture "id" (PrimaryKey (WithMetaInfo a) Identity) :> ReqBody '[JSON] (a Identity) :> Put '[JSON] NoContent)
           :<|> (Capture "id" (PrimaryKey (WithMetaInfo a) Identity) :> Delete '[JSON] NoContent)
       )

instance ToCapture (Capture "id" (PrimaryKey f Identity)) where
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
    MonadBeam be m,
    With '[MonadIO, MonadError ServerError] n
  ) =>
  (forall t. m t -> n t) ->
  DatabaseEntity be db (TableEntity (WithMetaInfo a)) ->
  ServerT (SimpleCRUDAPI path a) n
simpleCRUDServer q t = createOne q t :<|> readMany q t :<|> readOne q t :<|> updateOne q t :<|> deleteOne q t

simpleCRUDServerForHitmenBusiness dbGetter = simpleCRUDServer doPgQueryWithDebug (hitmenBusinessDb ^. dbGetter)

simpleCRUDServerForHitmenBusinessLite dbGetter = simpleCRUDServer doSqliteQueryWithDebug (hitmenBusinessDb ^. dbGetter)
