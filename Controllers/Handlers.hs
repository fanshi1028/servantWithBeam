{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Controllers.Handlers
  ( simpleCRUDServerForHandlers,
  )
where

import Colog (Message)
import Control.Monad.Except (MonadError)
import Data.Pool (Pool)
import Database.Beam.Postgres (Connection, Postgres)
import Databases.HitmenBusiness (HandlerB, HitmenBusinessDb)
import Servant (Handler, ServerError, ServerT)
import Universum
import Utils.CRUD (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)
import Utils.Types (MyServer)

simpleCRUDServerForHandlers :: ServerT (SimpleCRUDAPI path HandlerB) (MyServer Postgres HitmenBusinessDb Connection Message Handler)
simpleCRUDServerForHandlers = simpleCRUDServerForHitmenBusiness #_handlers
