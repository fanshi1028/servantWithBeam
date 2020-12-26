{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Controllers.ErasedMarks
  ( simpleCRUDServerForErasedMarks,
  )
where

import Control.Monad.Except (MonadError)
import Database.Beam.Postgres (Connection)
import Databases.HitmenBusiness (ErasedMarkT)
import Servant (ServerError, ServerT)
import Universum
import Utils.CRUD (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)

simpleCRUDServerForErasedMarks :: (MonadIO m, MonadError ServerError m) => ServerT (SimpleCRUDAPI path ErasedMarkT) (ReaderT Connection m)
simpleCRUDServerForErasedMarks = simpleCRUDServerForHitmenBusiness #_hbErasedMarks
