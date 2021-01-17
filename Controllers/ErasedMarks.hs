{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Controllers.ErasedMarks
  ( simpleCRUDServerForErasedMarks,
  )
where

import Colog (Message)
import Control.Monad.Except (MonadError)
import Data.Pool (Pool)
import Database.Beam.Postgres (Connection, Postgres)
import Databases.HitmenBusiness (ErasedMarkB, HitmenBusinessDb)
import Servant (Handler, ServerError, ServerT)
import Universum
import Utils.CRUD (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)
import Utils.Types (MyServer)

simpleCRUDServerForErasedMarks :: ServerT (SimpleCRUDAPI path ErasedMarkB) (MyServer Postgres HitmenBusinessDb Connection Message Handler)
simpleCRUDServerForErasedMarks = simpleCRUDServerForHitmenBusiness #_hbErasedMarks
