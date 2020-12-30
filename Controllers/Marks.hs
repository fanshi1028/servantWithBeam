{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Controllers.Marks
  ( simpleCRUDServerForMarks,
  )
where

import Control.Monad.Except (MonadError)
import Database.Beam.Postgres (Connection)
import Databases.HitmenBusiness (MarkB)
import Servant (ServerError, ServerT)
import Universum
import Utils.CRUD (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)

simpleCRUDServerForMarks ::
  ( With '[MonadIO, MonadError ServerError] m
  ) =>
  ServerT (SimpleCRUDAPI path MarkB) (ReaderT Connection m)
simpleCRUDServerForMarks = simpleCRUDServerForHitmenBusiness #_marks
