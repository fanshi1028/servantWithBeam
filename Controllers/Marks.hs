{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Controllers.Marks
  ( simpleCRUDServerForMarks,
  )
where

import Control.Monad.Except (MonadError)
import Database.Beam.Postgres (Connection)
import Databases.HitmenBusiness (MarkT)
import Servant (ServerError, ServerT)
import Universum
import Utils.CRUD (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)

simpleCRUDServerForMarks :: (MonadIO m, MonadError ServerError m) => ServerT (SimpleCRUDAPI path MarkT) (ReaderT Connection m)
simpleCRUDServerForMarks = simpleCRUDServerForHitmenBusiness #_marks
