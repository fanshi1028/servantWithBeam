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

simpleCRUDServerForHandlers :: (MonadIO m, MonadError ServerError m) => ServerT (SimpleCRUDAPI path HandlerB) (ReaderT Connection m)
simpleCRUDServerForHandlers = simpleCRUDServerForHitmenBusiness #_handlers
