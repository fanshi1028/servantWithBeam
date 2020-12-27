{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Controllers.Hitmen
  ( simpleCRUDServerForHitmen,
  )
where

import Control.Monad.Except (MonadError)
import Database.Beam.Postgres (Connection)
import Databases.HitmenBusiness (HitmanB)
import Servant (ServerError, ServerT)
import Universum
import Utils.CRUD (CreateRoute, DeleteRoute, ReadRoute, SimpleCRUDAPI, UpdateRoute, simpleCRUDServerForHitmenBusiness)

instance CreateRoute HitmanB

instance ReadRoute HitmanB

instance UpdateRoute HitmanB

instance DeleteRoute HitmanB

simpleCRUDServerForHitmen ::
  ( With '[MonadIO, MonadError ServerError] m,
    With '[CreateRoute, ReadRoute, UpdateRoute, DeleteRoute] HitmanB
  ) =>
  ServerT (SimpleCRUDAPI path HitmanB) (ReaderT Connection m)
simpleCRUDServerForHitmen = simpleCRUDServerForHitmenBusiness #_hitmen
