{-# LANGUAGE OverloadedLabels #-}

module Controllers.ErasedMarks
  ( simpleCRUDServerForErasedMarks,
  )
where

import Colog (Message)
import Database.Beam.Postgres (Connection, Postgres)
import Databases.HitmenBusiness (ErasedMarkB, HitmenBusinessDb)
import Servant (Handler, ServerT)
import Utils.CRUD (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)
import Utils.Types (MyServer)

simpleCRUDServerForErasedMarks :: ServerT (SimpleCRUDAPI path ErasedMarkB) (MyServer Postgres HitmenBusinessDb Connection Message Handler)
simpleCRUDServerForErasedMarks = simpleCRUDServerForHitmenBusiness #_hbErasedMarks
