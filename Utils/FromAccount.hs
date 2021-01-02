{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.FromAccount where

import Universum
import Utils.Meta (WithMetaInfo)
import Servant.Docs (ToSample)

class FromAccount userInfo a where
  data Base a :: (* -> *) -> *
  fromAccount :: WithMetaInfo userInfo Identity -> Base a Identity -> a Identity

