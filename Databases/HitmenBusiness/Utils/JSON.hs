module Databases.HitmenBusiness.Utils.JSON (noCamelOpt, flattenBaseMeta) where

import Data.Aeson (Options (fieldLabelModifier), Value (Object), camelTo2, defaultOptions)
import Data.HashMap.Strict (lookup)
import Universum

noCamelOpt = defaultOptions {fieldLabelModifier = maybe "Impossible! Empty field label" (camelTo2 '_' . tail) . nonEmpty}

flattenBaseMeta = \case
  (Object v) -> do
    base <- lookup "base" v >>= getHaspMap
    meta <- lookup "meta_info" v >>= getHaspMap
    return $ Object $ base <> meta
    where
      getHaspMap = \case
        (Object hm) -> Just hm
        _ -> Nothing
  _ -> Nothing
