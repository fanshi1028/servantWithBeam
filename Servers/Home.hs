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

import Controllers (SimpleCRUDAPI, SimpleCRUDHitmanAPI, simpleCRUDServerForHitmen')
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
import Data.Pool (Pool)

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
  -- serve @(APIWithDoc HomeAPI) Proxy $
  serveWithContext @(APIWithDoc HomeAPI) Proxy cfg $
    serveDocs @HomeAPI Proxy $
      -- hoistServer @(HomeAPI '[Cookie])
      hoistServerWithContext @HomeAPI @'[CookieSettings, JWTSettings]
        Proxy
        Proxy
        (usingReaderT conns)
        -- (crud #_handlers :<|> crud #_hitmen :<|> crud #_marks :<|> crud #_hbErasedMarks :<|> crud #_hbPursuingMarks)
        ( ( crud #_handlers
              :<|> simpleCRUDServerForHitmen' doPgQueryWithDebug
              :<|> crud #_marks
              :<|> crud #_hbErasedMarks
              :<|> crud #_hbPursuingMarks
          )
            :<|> authServer (hitmenBusinessDb ^. #_hbHandlersAccount) (hitmenBusinessDb ^. #_handlers) doPgQueryWithDebug cs jwts
        )
  where
    crud getter = simpleCRUDServerForHitmenBusiness getter

-- homeApp :: Connection -> Application
-- homeApp =
--   serve @(APIWithDoc HomeAPI)
--     Proxy
--     . serveDocs @HomeAPI Proxy
--     <$> simpleCRUDServerForHandlers
--       |:<|> simpleCRUDServerForHitmen
--       |:<|> simpleCRUDServerForMarks
--       |:<|> simpleCRUDServerForErasedMarks
--       |:<|> simpleCRUDServerForPursuingMarks
--   where
--     infixr 5 |:<|>
--     (|:<|>) = liftA2 (:<|>)
