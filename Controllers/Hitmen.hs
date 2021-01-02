{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.Hitmen
  ( simpleCRUDServerForHitmen,
    simpleCRUDServerForHitmen',
    SimpleCRUDHitmanAPI,
  )
where

import Chronos (Datetime)
import Control.Monad.Except (MonadError)
import Database.Beam (FromBackendRow, HasQBuilder, HasSqlEqualityCheck)
import Database.Beam.Backend (BeamSqlBackendCanSerialize, SqlNull)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning)
import Database.Beam.Postgres (Connection)
import Databases.HitmenBusiness (hitmenBusinessDb, HandlerB, HitmanB)
import Servant (ServerError, ServerT, (:<|>) ((:<|>)), (:>))
import Universum
import Utils.Account (ProtectApi, protected)
import Utils.CRUD (SimpleCRUDAPI, deleteOne, readMany, readOne, simpleCRUDServerForHitmenBusiness, updateOne)
import Utils.CRUD.CreateRoute (CreateApi, createOne')
import Utils.CRUD.DeleteRoute (DeleteApi)
import Utils.CRUD.ReadRoute (ReadManyApi, ReadOneApi)
import Utils.CRUD.UpdateRoute (UpdateApi)
import Utils.FromAccount (FromAccount (Base))

simpleCRUDServerForHitmen ::
  ( With '[MonadIO, MonadError ServerError] m
  ) =>
  ServerT (SimpleCRUDAPI path HitmanB) (ReaderT Connection m)
simpleCRUDServerForHitmen = simpleCRUDServerForHitmenBusiness #_hitmen

type HandlerAuths auths api = ProtectApi auths HandlerB api

type SimpleCRUDHitmanAPI auths =
  "hitman"
    :> ( HandlerAuths auths (CreateApi (Base HitmanB))
           :<|> ReadManyApi HitmanB
           :<|> ReadOneApi HitmanB
           :<|> HandlerAuths auths (UpdateApi HitmanB)
           :<|> HandlerAuths auths (DeleteApi HitmanB)
       )

simpleCRUDServerForHitmen' ::
  ( HasQBuilder be,
    BeamSqlBackendCanSerialize be (Maybe Datetime),
    With '[FromBackendRow be, BeamSqlBackendCanSerialize be] Text,
    With '[FromBackendRow be, HasSqlEqualityCheck be] Int32,
    FromBackendRow be Datetime,
    FromBackendRow be SqlNull,
    MonadBeamUpdateReturning be m,
    With '[MonadIO, MonadError ServerError] n
  ) =>
  (forall t. m t -> n t) ->
  ServerT (SimpleCRUDHitmanAPI auths) n
simpleCRUDServerForHitmen' doQuery =
  protected (createOne' doQuery table)
    :<|> readMany doQuery table
    :<|> readOne doQuery table
    :<|> protected (const $ updateOne doQuery table)
    :<|> protected (const $ deleteOne doQuery table)
  where table = hitmenBusinessDb ^. #_hitmen
