{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.Utils
  ( simpleCRUDServer,
    SimpleCRUDAPI,
    doPgQueryWithDebug,
    simpleCRUDServerForHitmenBusiness,
    -- doSqliteQueryWithDebug,
  )
where

import Database.Beam (FromBackendRow, MonadBeam, PrimaryKey)
import Database.Beam.Backend.SQL (BeamSqlBackendCanSerialize)
import Database.Beam.Postgres (Connection, Pg, runBeamPostgresDebug)
import Database.Beam.Query (HasSqlEqualityCheck, all_, delete, insert, insertExpressions, lookup_, runDelete, runInsert, runSelectReturningList, runSelectReturningOne, runUpdate, select, update, val_, (==.))
import Database.Beam.Query.Types (HasQBuilder)
import Database.Beam.Schema.Tables (Beamable, Database, DatabaseEntity, FieldsFulfillConstraint, Table, TableEntity, pk)
import Database.Beam.Sqlite.Connection (SqliteM, runBeamSqliteDebug)
import Databases.HitmenBusiness (hitmenBusinessDb)
import GHC.TypeLits (Symbol)
import Servant (Capture, Delete, Get, Handler, HasServer (ServerT), JSON, NoContent (NoContent), Post, Put, ReqBody, throwError, (:<|>) ((:<|>)), (:>))
import Servant.Docs (DocCapture (..), ToCapture (..))
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

instance ToCapture (Capture "id" (PrimaryKey f Identity)) where
  toCapture _ = DocCapture "id" "id"

simpleCRUDServer ::
  ( HasQBuilder be,
    Database be db,
    With [Beamable, Table, ToBase be] a,
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) a,
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) (PrimaryKey a),
    FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey a),
    FromBackendRow be (a Identity),
    MonadBeam be m
  ) =>
  (forall t. (m t -> ReaderT conn Handler t)) ->
  -- (forall t. m t ->  Handler t) ->
  DatabaseEntity be db (TableEntity a) ->
  ServerT (SimpleCRUDAPI path a) (ReaderT conn Handler)
-- Server (SimpleCRUDAPI path a)
simpleCRUDServer doQuery db = createOne :<|> readMany :<|> readOne :<|> updateOne :<|> deleteOne
  where
    createOne body = insertExpressions [fromBase body] & insert db & runInsert & doQuery >> return NoContent
    readMany = doQuery $ runSelectReturningList $ select $ all_ db
    readOne id = lookup_ db id & runSelectReturningOne & doQuery >>= maybe (throwError err404) return
    updateOne id body = update db (baseAsUpdate body) ((==. val_ id) . pk) & runUpdate & doQuery >> return NoContent
    deleteOne id = delete db ((==. val_ id) . pk) & runDelete & doQuery >> return NoContent

doPgQueryWithDebug :: (MonadIO m) => (Pg a -> ReaderT Connection m a)
doPgQueryWithDebug = ReaderT . (liftIO <$>) <$> flip (runBeamPostgresDebug putStrLn)

-- doPgQueryWithDebug conn = liftIO <$> runBeamPostgresDebug putStrLn conn

-- doSqliteQueryWithDebug :: (MonadIO m) => (SqliteM a -> ReaderT Connection m a)
-- doSqliteQueryWithDebug = ReaderT . (liftIO <$>) <$> flip (runBeamSqliteDebug putStrLn)

-- doSqliteQueryWithDebug conn = liftIO <$> runBeamSqliteDebug putStrLn conn

simpleCRUDServerForHitmenBusiness dbGetter = simpleCRUDServer doPgQueryWithDebug (hitmenBusinessDb ^. dbGetter)

-- simpleCRUDServerForHitmenBusiness dbGetter conn = simpleCRUDServer (doPgQueryWithDebug conn) (hitmenBusinessDb ^. dbGetter)
