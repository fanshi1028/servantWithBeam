{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.FromAccount where

import Servant.Docs (ToSample)
import Universum
import Utils.Meta (WithMetaInfo)

class FromAccount userInfo a where
  data Base a :: (* -> *) -> *
  fromAccount :: WithMetaInfo userInfo Identity -> Base a Identity -> a Identity
