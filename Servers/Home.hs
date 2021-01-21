{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Servers.Home
  ( HomeAPI,
    HomeAPI',
    homeApp,
  )
where

import Colog (LogAction, Message)
import Controllers (SimpleCRUDAPI, SimpleCRUDHitmanAPI, simpleCRUDServerForHitmen')
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple (Connection)
import Databases.HitmenBusiness (ErasedMarkB, HandlerB, HitmenBusinessDb, MarkB, PursuingMarkB)
import Servant (Application, Context (EmptyContext, (:.)), Handler, HasServer (hoistServerWithContext), serveWithContext, (:<|>) ((:<|>)))
import Servant.Auth.Docs ()
import Servant.Auth.Server (Cookie, CookieSettings, JWT, JWTSettings)
import Universum
import Utils.Account.Auth (AuthApi, authServer)
import Utils.CRUD (simpleCRUDServerForHitmenBusiness)
import Utils.Docs (APIWithDoc, serveDocs)
import Utils.QueryRunner (doPgQueryWithDebug)
import Utils.Types (Env (Env, _cs, _jwts), MyServer (unMyServer))

type HomeAPI' auths =
  SimpleCRUDAPI "handlers" HandlerB
    :<|> SimpleCRUDHitmanAPI auths
    :<|> SimpleCRUDAPI "marks" MarkB
    :<|> SimpleCRUDAPI "erased_marks" ErasedMarkB
    :<|> SimpleCRUDAPI "pursuing_marks" PursuingMarkB

type HomeAPI = HomeAPI' '[JWT, Cookie] :<|> AuthApi HandlerB

homeApp ::
  ( Env
      Postgres
      HitmenBusinessDb
      Connection
      (LogAction (MyServer Postgres HitmenBusinessDb Connection Message Handler) Message)
  ) ->
  Application
homeApp env@Env {_cs, _jwts} = do
  serveWithContext @(APIWithDoc HomeAPI) Proxy (_cs :. _jwts :. EmptyContext) $
    serveDocs @HomeAPI Proxy $
      hoistServerWithContext @HomeAPI @'[CookieSettings, JWTSettings]
        Proxy
        Proxy
        (usingReaderT env . unMyServer)
        $ ( crud #_handlers
              :<|> simpleCRUDServerForHitmen' doPgQueryWithDebug
              :<|> crud #_marks
              :<|> crud #_hbErasedMarks
              :<|> crud #_hbPursuingMarks
          )
          :<|> authServer (view #_hbHandlersAccount) (view #_handlers) doPgQueryWithDebug
  where
    crud getter = simpleCRUDServerForHitmenBusiness getter
