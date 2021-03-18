{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Migration.Databases.HitmenBusiness.Handlers where

import Database.Beam.AutoMigrate (HasColumnType(..))
import Utils.Account.Login (LoginId)
import Databases.HitmenBusiness.Handlers (HandlerB)
import Databases.HitmenBusiness.Utils.Types (Codename)
import Universum ( Proxy(Proxy) )

instance HasColumnType Codename => HasColumnType (LoginId HandlerB) where
  defaultColumnType _ = defaultColumnType @Codename Proxy
