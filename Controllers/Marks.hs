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
import Utils.CRUD (CreateRoute, DeleteRoute, ReadRoute, SimpleCRUDAPI, UpdateRoute, simpleCRUDServerForHitmenBusiness)

instance CreateRoute MarkB

instance ReadRoute MarkB

instance UpdateRoute MarkB

instance DeleteRoute MarkB

simpleCRUDServerForMarks ::
  ( With '[MonadIO, MonadError ServerError] m,
    With '[CreateRoute, ReadRoute, UpdateRoute, DeleteRoute] MarkB
  ) =>
  ServerT (SimpleCRUDAPI path MarkB) (ReaderT Connection m)
simpleCRUDServerForMarks = simpleCRUDServerForHitmenBusiness #_marks
