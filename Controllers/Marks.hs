{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Controllers.Marks
  ( simpleCRUDServerForMarks,
  )
where

import Colog (Message)
import Control.Monad.Except (MonadError)
import Data.Pool (Pool)
import Database.Beam.Postgres (Connection, Postgres)
import Databases.HitmenBusiness (HitmenBusinessDb, MarkB)
import Utils.Types (MyServer)
import Servant (Handler, ServerError, ServerT)
import Universum
import Utils.CRUD (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)

simpleCRUDServerForMarks :: ServerT (SimpleCRUDAPI path MarkB) (MyServer Postgres HitmenBusinessDb Connection Message Handler)
simpleCRUDServerForMarks = simpleCRUDServerForHitmenBusiness #_marks
