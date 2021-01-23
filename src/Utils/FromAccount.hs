{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.FromAccount where

import Universum
import Utils.Meta (WithMetaInfo)

class FromAccount userInfo a where
  data Base a :: (Type -> Type) -> Type
  fromAccount :: WithMetaInfo userInfo Identity -> Base a Identity -> a Identity
