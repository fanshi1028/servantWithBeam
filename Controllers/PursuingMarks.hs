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
import Utils.CRUD (CreateRoute, DeleteRoute, ReadRoute, SimpleCRUDAPI, UpdateRoute, simpleCRUDServerForHitmenBusiness)

instance CreateRoute PursuingMarkB

instance ReadRoute PursuingMarkB

instance UpdateRoute PursuingMarkB

instance DeleteRoute PursuingMarkB

simpleCRUDServerForPursuingMarks ::
  ( With '[MonadIO, MonadError ServerError] m,
    With '[CreateRoute, ReadRoute, UpdateRoute, DeleteRoute] PursuingMarkB
  ) =>
  ServerT (SimpleCRUDAPI path PursuingMarkB) (ReaderT Connection m)
simpleCRUDServerForPursuingMarks = simpleCRUDServerForHitmenBusiness #_hbPursuingMarks
