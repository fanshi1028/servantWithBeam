{-# LANGUAGE OverloadedLabels #-}

module Controllers.Marks
  ( simpleCRUDServerForMarks,
  )
where

import Colog (Message)
import Database.Beam.Postgres (Connection, Postgres)
import Databases.HitmenBusiness (HitmenBusinessDb, MarkB)
import Utils.Types (MyServer)
import Servant (Handler, ServerT)
import Utils.CRUD (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)

simpleCRUDServerForMarks :: ServerT (SimpleCRUDAPI path MarkB) (MyServer Postgres HitmenBusinessDb Connection Message Handler)
simpleCRUDServerForMarks = simpleCRUDServerForHitmenBusiness #_marks
