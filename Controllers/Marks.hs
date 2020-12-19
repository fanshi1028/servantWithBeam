{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Controllers.Marks
  ( simpleCRUDServerForMarks,
  )
where

import Control.Monad.Except (MonadError)
import Controllers.Utils (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)
import Database.Beam.Postgres (Connection)
import Databases.HitmenBusiness (MarkT)
import Servant (ServerError, ServerT)

simpleCRUDServerForMarks :: (MonadIO m, MonadError ServerError m) => ServerT (SimpleCRUDAPI path MarkT) (ReaderT Connection m)
simpleCRUDServerForMarks = simpleCRUDServerForHitmenBusiness #_marks
