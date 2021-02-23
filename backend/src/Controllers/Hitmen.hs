{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.Hitmen
  ( simpleCRUDServerForHitmen,
    simpleCRUDServerForHitmen',
    SimpleCRUDHitmanAPI,
  )
where

import Chronos (Datetime)
import Colog (Message)
import Control.Natural (type (~>))
import Database.Beam (FromBackendRow, HasQBuilder, HasSqlEqualityCheck)
import Database.Beam.Backend (BeamSqlBackendCanSerialize, SqlNull)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning)
import Database.Beam.Postgres (Connection, Postgres)
import Databases.HitmenBusiness (HandlerB, HitmanB, HitmenBusinessDb)
import Servant (Handler, ServerT, (:<|>) ((:<|>)), (:>))
import Universum
import Utils.Account (ProtectApi, protected)
import Utils.CRUD (SimpleCRUDAPI, deleteOne, readMany, readOne, simpleCRUDServerForHitmenBusiness, updateOne)
import Utils.CRUD.CreateRoute (CreateApi, createOne')
import Utils.CRUD.DeleteRoute (DeleteApi)
import Utils.CRUD.ReadRoute (ReadManyApi, ReadOneApi)
import Utils.CRUD.UpdateRoute (UpdateApi)
import Utils.FromAccount (FromAccount (Base))
import Utils.Types (MyServer)

simpleCRUDServerForHitmen :: ServerT (SimpleCRUDAPI path HitmanB) (MyServer Postgres HitmenBusinessDb Connection Message Handler)
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
    MonadBeamUpdateReturning be m
  ) =>
  (m ~> MyServer be HitmenBusinessDb Connection msg Handler) ->
  ServerT (SimpleCRUDHitmanAPI auths) (MyServer be HitmenBusinessDb Connection msg Handler)
simpleCRUDServerForHitmen' doQuery =
  protected (createOne' doQuery tableGet)
    :<|> readMany doQuery tableGet
    :<|> readOne doQuery tableGet
    :<|> protected (const $ updateOne doQuery tableGet)
    :<|> protected (const $ deleteOne doQuery tableGet)
  where
    tableGet = view #_hitmen
