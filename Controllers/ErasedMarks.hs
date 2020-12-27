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
import Utils.CRUD (CreateRoute, DeleteRoute, ReadRoute, SimpleCRUDAPI, UpdateRoute, simpleCRUDServerForHitmenBusiness)

instance CreateRoute ErasedMarkB

instance ReadRoute ErasedMarkB

instance UpdateRoute ErasedMarkB

instance DeleteRoute ErasedMarkB

simpleCRUDServerForErasedMarks ::
  ( With '[MonadIO, MonadError ServerError] m,
    With '[CreateRoute, ReadRoute, UpdateRoute, DeleteRoute] ErasedMarkB
  ) =>
  ServerT (SimpleCRUDAPI path ErasedMarkB) (ReaderT Connection m)
simpleCRUDServerForErasedMarks = simpleCRUDServerForHitmenBusiness #_hbErasedMarks
