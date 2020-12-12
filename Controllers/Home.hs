{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
-- import Database.SQLite.Simple (Connection)

import Data.Text.Lazy (pack)
import Database.PostgreSQL.Simple (Connection, QueryError (..), qeMessage)
import Databases.HitmenBusiness.ErasedMarks (ErasedMarkT)
import Databases.HitmenBusiness.Handlers (HandlerT)
import Databases.HitmenBusiness.Hitmen (HitmanT)
import Databases.HitmenBusiness.Marks (MarkT)
import Databases.HitmenBusiness.PursuingMarks (PursuingMarkT)
import Lens.Micro ((&), (.~))
import Servant (Proxy (Proxy), err500, throwError, (:<|>) ((:<|>)))
import Servant.Server (Application, serve)

type HomeAPI =
  SimpleCRUDAPI "handlers" HandlerT
    :<|> SimpleCRUDAPI "hitmen" HitmanT
    :<|> SimpleCRUDAPI "marks" MarkT
    :<|> SimpleCRUDAPI "erased_marks" ErasedMarkT
    :<|> SimpleCRUDAPI "pursuing_marks" PursuingMarkT

homeApp :: Connection -> Application
homeApp =
  serve @HomeAPI
    Proxy
    <$> simpleCRUDServerForHandler
    |:<|> simpleCRUDServerForHitman
    |:<|> simpleCRUDServerForHitmenBusiness #_marks
    |:<|> simpleCRUDServerForHitmenBusiness #_hbErasedMarks
    |:<|> simpleCRUDServerForHitmenBusiness #_hbPursuingMarks
  where
    infixr 5 |:<|>
    (|:<|>) = liftA2 (:<|>)

-- homeApp :: Connection -> Application
-- homeApp =
--   serve @(SimpleCRUDAPI "handlers" HandlerT)
--     Proxy
--     <$> simpleCRUDServerForHandler

-- handlerErr = \case
--   QueryError e _ -> throwError err500 & #errBody .~ e
