{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Servers.Home
  ( HomeAPI,
    HomeAPI',
    homeApp,
  )
where

import Colog (richMessageAction)
import Controllers (SimpleCRUDAPI, SimpleCRUDHitmanAPI, simpleCRUDServerForHitmen')
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Databases.HitmenBusiness (ErasedMarkB, HandlerB, MarkB, PursuingMarkB, hitmenBusinessDb)
import Servant (Application, Context, ErrorFormatters, HasContextEntry, HasServer (hoistServerWithContext), serveWithContext, (:<|>) ((:<|>)), type (.++))
import Servant.Auth.Docs ()
import Servant.Auth.Server (Cookie, CookieSettings, JWT, JWTSettings)
import Servant.Server (DefaultErrorFormatters)
import Universum
import Utils.Account.Auth (AuthApi, authServer)
import Utils.CRUD (simpleCRUDServerForHitmenBusiness)
import Utils.Docs (APIWithDoc, serveDocs)
import Utils.QueryRunner (doPgQueryWithDebug)
import Utils.Types (Env (Env), MyServer (unMyServer))

type HomeAPI' auths =
  ( SimpleCRUDAPI "handlers" HandlerB
      :<|> SimpleCRUDHitmanAPI auths
      :<|> SimpleCRUDAPI "marks" MarkB
      :<|> SimpleCRUDAPI "erased_marks" ErasedMarkB
      :<|> SimpleCRUDAPI "pursuing_marks" PursuingMarkB
  )

type HomeAPI = HomeAPI' '[JWT, Cookie] :<|> AuthApi HandlerB

homeApp ::
  ( HasContextEntry context JWTSettings,
    HasContextEntry context CookieSettings,
    HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  Context context ->
  CookieSettings ->
  JWTSettings ->
  Pool Connection ->
  Application
homeApp cfg cs jwts conns = do
  serveWithContext @(APIWithDoc HomeAPI) Proxy cfg $
    serveDocs @HomeAPI Proxy $
      hoistServerWithContext @HomeAPI @'[CookieSettings, JWTSettings]
        Proxy
        Proxy
        (usingReaderT (Env richMessageAction cs jwts conns hitmenBusinessDb) . unMyServer)
        ( ( crud #_handlers
              :<|> simpleCRUDServerForHitmen' doPgQueryWithDebug
              :<|> crud #_marks
              :<|> crud #_hbErasedMarks
              :<|> crud #_hbPursuingMarks
          )
            :<|> authServer (view #_hbHandlersAccount) (view #_handlers) doPgQueryWithDebug
        )
  where
    crud getter = simpleCRUDServerForHitmenBusiness getter
