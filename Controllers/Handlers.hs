{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Controllers.Handlers
  ( simpleCRUDServerForHandlers,
  )
where

import Control.Monad.Except (MonadError)
import Database.Beam.Postgres (Connection)
import Databases.HitmenBusiness (HandlerB)
import Servant (ServerError, ServerT)
import Universum
import Utils.CRUD (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)
import Data.Pool (Pool)

simpleCRUDServerForHandlers ::
  ( With '[MonadIO, MonadError ServerError] m
  ) =>
  ServerT (SimpleCRUDAPI path HandlerB) (ReaderT (Pool Connection) m)
simpleCRUDServerForHandlers = simpleCRUDServerForHitmenBusiness #_handlers
