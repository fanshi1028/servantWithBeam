{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Servers.Home
  ( HomeAPI,
    homeApp,
  )
where

import Control.Monad.Reader (ReaderT (runReaderT))
import Controllers (SimpleCRUDAPI, simpleCRUDServerForErasedMarks, simpleCRUDServerForHandler, simpleCRUDServerForHitman, simpleCRUDServerForMarks, simpleCRUDServerForPursuingMarks)
import Database.PostgreSQL.Simple (Connection)
import Databases.HitmenBusiness (ErasedMarkT, HandlerT, HitmanT, MarkT, PursuingMarkT)
import Servant (Application, Proxy (Proxy), hoistServer, serve, (:<|>) ((:<|>)))
import Util.Docs (APIWithDoc, serveDocs)

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
