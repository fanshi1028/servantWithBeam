module Databases.HitmenBusiness.Utils.JSON (noCamelOpt, flattenBase) where

import Data.Aeson (Options (fieldLabelModifier), Value (Object), camelTo2, defaultOptions)
import Data.HashMap.Strict (delete, lookup)
import Universum

noCamelOpt = defaultOptions {fieldLabelModifier = maybe "Impossible! Empty field label" (camelTo2 '_' . tail) . nonEmpty}

flattenBase obj@(Object v) =
  maybe
    obj
    ( \case
        (Object base) -> Object $ delete "base" v <> base
        _ -> obj
    )
    (lookup "base" v)
