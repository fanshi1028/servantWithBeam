{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.Home
  ( HomeAPI,
    homeApp,
  )
where

import Control.Applicative (Applicative (liftA2))
import Controllers.Handlers (simpleCRUDServerForHandler)
import Controllers.Hitmen (simpleCRUDServerForHitman)
import Controllers.Util (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)
import Database.PostgreSQL.Simple (Connection)
import Databases.HitmenBusiness.ErasedMarks (ErasedMarkT)
import Databases.HitmenBusiness.Handlers (HandlerT)
import Databases.HitmenBusiness.Hitmen (HitmanT)
import Databases.HitmenBusiness.Marks (MarkT)
import Databases.HitmenBusiness.PursuingMarks (PursuingMarkT)
import Servant (Proxy (Proxy), (:<|>) ((:<|>)))
import Servant.Server (Application, serve)
import Util.Docs (APIWithDoc, serveDocs)

type HomeAPI =
  SimpleCRUDAPI "handlers" HandlerT
    :<|> SimpleCRUDAPI "hitmen" HitmanT
    :<|> SimpleCRUDAPI "marks" MarkT
    :<|> SimpleCRUDAPI "erased_marks" ErasedMarkT
    :<|> SimpleCRUDAPI "pursuing_marks" PursuingMarkT

homeApp :: Connection -> Application
homeApp =
  serve @(APIWithDoc HomeAPI)
    Proxy
    . serveDocs @HomeAPI Proxy
    <$> simpleCRUDServerForHandler
      |:<|> simpleCRUDServerForHitman
      |:<|> simpleCRUDServerForHitmenBusiness #_marks
      |:<|> simpleCRUDServerForHitmenBusiness #_hbErasedMarks
      |:<|> simpleCRUDServerForHitmenBusiness #_hbPursuingMarks
  where
    infixr 5 |:<|>
    (|:<|>) = liftA2 (:<|>)
