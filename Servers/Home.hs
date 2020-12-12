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
    simpleCRUDServerForHandler,
    simpleCRUDServerForHitman,
    simpleCRUDServerForMarks,
    simpleCRUDServerForPursuingMarks,
  )
import Database.PostgreSQL.Simple (Connection)
import Databases.HitmenBusiness (ErasedMarkT, HandlerT, HitmanT, MarkT, PursuingMarkT)
import Servant (Application, hoistServer, serve, (:<|>) ((:<|>)))
import Utils.Docs (APIWithDoc, serveDocs)

type HomeAPI =
  SimpleCRUDAPI "handlers" HandlerT
    :<|> SimpleCRUDAPI "hitmen" HitmanT
    :<|> SimpleCRUDAPI "marks" MarkT
    :<|> SimpleCRUDAPI "erased_marks" ErasedMarkT
    :<|> SimpleCRUDAPI "pursuing_marks" PursuingMarkT

homeApp :: Connection -> Application
homeApp conn =
  serve @(APIWithDoc HomeAPI) Proxy $
    serveDocs @HomeAPI Proxy $
      hoistServer @HomeAPI
        Proxy
        (`runReaderT` conn)
        ( simpleCRUDServerForHandler
            :<|> simpleCRUDServerForHitman
            :<|> simpleCRUDServerForMarks
            :<|> simpleCRUDServerForErasedMarks
            :<|> simpleCRUDServerForPursuingMarks
        )

-- homeApp :: Connection -> Application
-- homeApp =
--   serve @(APIWithDoc HomeAPI)
--     Proxy
--     . serveDocs @HomeAPI Proxy
--     <$> simpleCRUDServerForHandler
--       |:<|> simpleCRUDServerForHitman
--       |:<|> simpleCRUDServerForMarks
--       |:<|> simpleCRUDServerForErasedMarks
--       |:<|> simpleCRUDServerForPursuingMarks
--   where
--     infixr 5 |:<|>
--     (|:<|>) = liftA2 (:<|>)
