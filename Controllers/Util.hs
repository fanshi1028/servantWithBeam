{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.Util
  ( simpleCRUDServer,
    SimpleCRUDAPI,
    doPgQueryWithDebug,
    simpleCRUDServerForHitmenBusiness,
    doSqliteQueryWithDebug,
  )
where

import Database.Beam (FromBackendRow, Identity, MonadBeam, PrimaryKey, liftIO)
import Database.Beam.Backend.SQL (BeamSqlBackendCanSerialize)
import Database.Beam.Postgres (runBeamPostgresDebug)
import Database.Beam.Query (HasSqlEqualityCheck, all_, delete, insert, insertExpressions, lookup_, runDelete, runInsert, runSelectReturningList, runSelectReturningOne, runUpdate, select, update, val_, (==.))
import Database.Beam.Query.Types (HasQBuilder)
import Database.Beam.Schema.Tables (Beamable, Database, DatabaseEntity, FieldsFulfillConstraint, Table, TableEntity, pk)
import Database.Beam.Sqlite.Connection (runBeamSqliteDebug)
import Databases.HitmenBusiness (hitmenBusinessDb)
import GHC.TypeLits (Symbol)
import Lens.Micro ((&), (^.))
import Servant (Capture, Delete, Get, Handler, JSON, NoContent (NoContent), Post, Put, ReqBody, throwError, (:<|>) ((:<|>)), (:>))
import Servant.Server (Server)
import Servant.Server.Internal.ServerError (err404)
import Typeclass.Base (ToBase (..))

type SimpleCRUDAPI (path :: Symbol) a =
  path
    :> ( (ReqBody '[JSON] (Base a Identity) :> Post '[JSON] NoContent)
           :<|> Get '[JSON] [a Identity]
           :<|> (Capture "id" (PrimaryKey a Identity) :> Get '[JSON] (a Identity))
           :<|> (Capture "id" (PrimaryKey a Identity) :> ReqBody '[JSON] (Base a Identity) :> Put '[JSON] NoContent)
           :<|> (Capture "id" (PrimaryKey a Identity) :> Delete '[JSON] NoContent)
       )

simpleCRUDServer ::
  ( Beamable a,
    MonadBeam be m,
    FromBackendRow be (a Identity),
    Database be db,
    Table a,
    HasQBuilder be,
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) (PrimaryKey a),
    FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey a),
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) a,
    ToBase be a
  ) =>
  (forall t. (m t -> Handler t)) ->
  DatabaseEntity be db (TableEntity a) ->
  Server (SimpleCRUDAPI path a)
simpleCRUDServer doQuery db = createOne :<|> readMany :<|> readOne :<|> updateOne :<|> deleteOne
  where
    createOne body = insertExpressions [fromBase body] & insert db & runInsert & doQuery >> return NoContent
    readMany = doQuery $ runSelectReturningList $ select $ all_ db
    readOne id = lookup_ db id & runSelectReturningOne & doQuery >>= maybe (throwError err404) return
    updateOne id body = update db (baseAsUpdate body) ((==. val_ id) . pk) & runUpdate & doQuery >> return NoContent
    deleteOne id = delete db ((==. val_ id) . pk) & runDelete & doQuery >> return NoContent

doPgQueryWithDebug conn = liftIO <$> runBeamPostgresDebug putStrLn conn

doSqliteQueryWithDebug conn = liftIO <$> runBeamSqliteDebug putStrLn conn

simpleCRUDServerForHitmenBusiness dbGetter conn = simpleCRUDServer (doPgQueryWithDebug conn) (hitmenBusinessDb ^. dbGetter)
