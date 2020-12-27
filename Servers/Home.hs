{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Servers.Home
  ( HomeAPI,
    homeApp,
  )
where

import Controllers
  ( SimpleCRUDAPI,
    simpleCRUDServerForErasedMarks,
    simpleCRUDServerForHandlers,
    simpleCRUDServerForHitmen,
    simpleCRUDServerForMarks,
    simpleCRUDServerForPursuingMarks,
  )
import Database.PostgreSQL.Simple (Connection)
import Servant (Application, hoistServer, serve, (:<|>) ((:<|>)))
import Databases.HitmenBusiness (ErasedMarkB, HandlerB, HitmanB, MarkB, PursuingMarkB)
import Universum
import Utils.Docs (APIWithDoc, serveDocs)

type HomeAPI =
  SimpleCRUDAPI "handlers" HandlerB
    :<|> SimpleCRUDAPI "hitmen" HitmanB
    :<|> SimpleCRUDAPI "marks" MarkB
    :<|> SimpleCRUDAPI "erased_marks" ErasedMarkB
    :<|> SimpleCRUDAPI "pursuing_marks" PursuingMarkB


homeApp :: Connection -> Application
homeApp conn =
  serve @(APIWithDoc HomeAPI) Proxy $
    serveDocs @HomeAPI Proxy $
      hoistServer @HomeAPI
        Proxy
        (usingReaderT conn)
        ( simpleCRUDServerForHandlers
            :<|> simpleCRUDServerForHitmen
            :<|> simpleCRUDServerForMarks
            :<|> simpleCRUDServerForErasedMarks
            :<|> simpleCRUDServerForPursuingMarks
        )

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
