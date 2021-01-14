{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Controllers.ErasedMarks
  ( simpleCRUDServerForErasedMarks,
  )
where

import Control.Monad.Except (MonadError)
import Database.Beam.Postgres (Connection)
import Databases.HitmenBusiness (ErasedMarkB)
import Servant (ServerError, ServerT)
import Universum
import Utils.CRUD (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)
import Data.Pool (Pool)

simpleCRUDServerForErasedMarks ::
  ( With '[MonadIO, MonadError ServerError] m
  ) =>
  ServerT (SimpleCRUDAPI path ErasedMarkB) (ReaderT (Pool Connection) m)
simpleCRUDServerForErasedMarks = simpleCRUDServerForHitmenBusiness #_hbErasedMarks
