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
-- import Database.SQLite.Simple (Connection)

import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Connection, QueryError (..), qeMessage)
import Databases.HitmenBusiness.ErasedMarks (ErasedMarkT)
import Databases.HitmenBusiness.Handlers (HandlerT)
import Databases.HitmenBusiness.Hitmen (HitmanT)
import Databases.HitmenBusiness.Marks (MarkT)
import Databases.HitmenBusiness.PursuingMarks (PursuingMarkT)
import Lens.Micro ((&), (.~))
import Network.HTTP.Types (ok200)
import Network.Wai (responseLBS)
import Servant (Proxy (Proxy), Raw, Tagged (Tagged), err500, throwError, (:<|>) ((:<|>)))
import Servant.Docs (docs, markdown)
import Servant.Server (Application, serve)
import Util.Docs (DocAPI, serveDocs)

type HomeAPI =
  SimpleCRUDAPI "handlers" HandlerT
    :<|> SimpleCRUDAPI "hitmen" HitmanT
    :<|> SimpleCRUDAPI "marks" MarkT
    :<|> SimpleCRUDAPI "erased_marks" ErasedMarkT
    :<|> SimpleCRUDAPI "pursuing_marks" PursuingMarkT

homeApp :: Connection -> Application
homeApp =
  serve @(DocAPI HomeAPI)
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
