module Databases.HitmenBusiness.Utils.JSON (noCamelOpt, flatten, noCamelOpt') where

import Data.Aeson (Object, Options (fieldLabelModifier), Value (Object), camelTo2, defaultOptions)
import Data.HashMap.Strict (fromList, lookup)
import Universum

noCamelOptInternal strip = defaultOptions {fieldLabelModifier = maybe "Impossible! Empty field label" (camelTo2 '_' . strip) . nonEmpty}

noCamelOpt = noCamelOptInternal tail

noCamelOpt' = noCamelOptInternal init

flatten1 :: Text -> Value -> Maybe Object
flatten1 key = \case
  (Object hm) -> Just hm
  s -> Just $ fromList [(key, s)]

flatten key1 key2 = \case
  (Object v) -> do
    let tryFlatten k = lookup k v >>= flatten1 k
    base <- tryFlatten key1
    meta <- tryFlatten key2
    return $ Object $ base <> meta
  _ -> Nothing

