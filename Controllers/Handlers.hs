{-# LANGUAGE OverloadedLabels #-}

module Controllers.Handlers
  ( simpleCRUDServerForHandlers,
  )
where

import Colog (Message)
import Database.Beam.Postgres (Connection, Postgres)
import Databases.HitmenBusiness (HandlerB, HitmenBusinessDb)
import Servant (Handler, ServerT)
import Utils.CRUD (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)
import Utils.Types (MyServer)

simpleCRUDServerForHandlers :: ServerT (SimpleCRUDAPI path HandlerB) (MyServer Postgres HitmenBusinessDb Connection Message Handler)
simpleCRUDServerForHandlers = simpleCRUDServerForHitmenBusiness #_handlers
