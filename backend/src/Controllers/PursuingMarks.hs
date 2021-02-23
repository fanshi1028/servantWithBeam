{-# LANGUAGE OverloadedLabels #-}

module Controllers.PursuingMarks
  ( simpleCRUDServerForPursuingMarks,
  )
where

import Colog (Message)
import Database.Beam.Postgres (Connection, Postgres)
import Databases.HitmenBusiness (HitmenBusinessDb, PursuingMarkB)
import Servant (Handler, ServerT)
import Utils.CRUD (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)
import Utils.Types (MyServer)

simpleCRUDServerForPursuingMarks :: ServerT (SimpleCRUDAPI path PursuingMarkB) (MyServer Postgres HitmenBusinessDb Connection Message Handler)
simpleCRUDServerForPursuingMarks = simpleCRUDServerForHitmenBusiness #_hbPursuingMarks
