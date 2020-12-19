{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Controllers.PursuingMarks
  ( simpleCRUDServerForPursuingMarks,
  )
where

import Control.Monad.Except (MonadError)
import Controllers.Utils (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)
import Database.Beam.Postgres (Connection)
import Databases.HitmenBusiness (PursuingMarkT)
import Servant (ServerError, ServerT)

simpleCRUDServerForPursuingMarks :: (MonadIO m, MonadError ServerError m) => ServerT (SimpleCRUDAPI path PursuingMarkT) (ReaderT Connection m)
simpleCRUDServerForPursuingMarks = simpleCRUDServerForHitmenBusiness #_hbPursuingMarks
