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
import Utils.CRUD (CreateRoute, DeleteRoute, ReadRoute, SimpleCRUDAPI, UpdateRoute, simpleCRUDServerForHitmenBusiness)

instance CreateRoute HandlerB

instance ReadRoute HandlerB

instance UpdateRoute HandlerB

instance DeleteRoute HandlerB

simpleCRUDServerForHandlers ::
  ( With '[MonadIO, MonadError ServerError] m,
    With '[CreateRoute, ReadRoute, UpdateRoute, DeleteRoute] HandlerB
  ) =>
  ServerT (SimpleCRUDAPI path HandlerB) (ReaderT Connection m)
simpleCRUDServerForHandlers = simpleCRUDServerForHitmenBusiness #_handlers
