{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Controllers.PursuingMarks
  ( simpleCRUDServerForPursuingMarks,
  )
where

import Control.Monad.Except (MonadError)
import Database.Beam.Postgres (Connection)
import Databases.HitmenBusiness (PursuingMarkB)
import Servant (ServerError, ServerT)
import Universum
import Utils.CRUD (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)
import Data.Pool (Pool)

simpleCRUDServerForPursuingMarks ::
  (With '[MonadIO, MonadError ServerError] m) =>
  ServerT (SimpleCRUDAPI path PursuingMarkB) (ReaderT (Pool Connection) m)
simpleCRUDServerForPursuingMarks = simpleCRUDServerForHitmenBusiness #_hbPursuingMarks
