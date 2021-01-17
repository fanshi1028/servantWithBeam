{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Controllers.PursuingMarks
  ( simpleCRUDServerForPursuingMarks,
  )
where

import Colog (Message)
import Control.Monad.Except (MonadError)
import Data.Pool (Pool)
import Database.Beam.Postgres (Connection, Postgres)
import Databases.HitmenBusiness (HitmenBusinessDb, PursuingMarkB)
import Servant (Handler, ServerError, ServerT)
import Universum
import Utils.CRUD (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)
import Utils.Types (MyServer)

simpleCRUDServerForPursuingMarks :: ServerT (SimpleCRUDAPI path PursuingMarkB) (MyServer Postgres HitmenBusinessDb Connection Message Handler)
simpleCRUDServerForPursuingMarks = simpleCRUDServerForHitmenBusiness #_hbPursuingMarks
