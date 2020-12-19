{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Controllers.Handlers
  ( simpleCRUDServerForHandlers,
  )
where

import Control.Monad.Except (MonadError)
import Controllers.Utils (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)
import Database.Beam.Postgres (Connection)
import Databases.HitmenBusiness (HandlerT)
import Servant (ServerError, ServerT)

simpleCRUDServerForHandlers :: (MonadIO m, MonadError ServerError m) => ServerT (SimpleCRUDAPI path HandlerT) (ReaderT Connection m)
simpleCRUDServerForHandlers = simpleCRUDServerForHitmenBusiness #_handlers
