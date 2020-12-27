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
import Utils.CRUD (SimpleCRUDAPI, simpleCRUDServerForHitmenBusiness)

simpleCRUDServerForHitmen :: (MonadIO m, MonadError ServerError m) => ServerT (SimpleCRUDAPI path HitmanB) (ReaderT Connection m)
simpleCRUDServerForHitmen = simpleCRUDServerForHitmenBusiness #_hitmen
