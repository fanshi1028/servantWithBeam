module Databases.HitmenBusiness.Utils.JSON (noCamelOpt, flatten, noCamelOpt') where

import Data.Aeson (Object, Options (fieldLabelModifier), Value (Object), camelTo2, defaultOptions)
import Data.HashMap.Strict (fromList, lookup)
import Universum

noCamelOptInternal :: (NonEmpty Char -> String) -> Options
noCamelOptInternal strip = defaultOptions {fieldLabelModifier = maybe "Impossible! Empty field label from using noCamelOpt" (camelTo2 '_' . strip) . nonEmpty}

noCamelOpt :: Options
noCamelOpt = noCamelOptInternal tail

noCamelOpt' :: Options
noCamelOpt' = noCamelOptInternal $ maybe "Impossible! Empty field label from using noCamelOpt'" init . nonEmpty . tail

flatten1 :: Text -> Value -> Maybe Object
flatten1 key = \case
  (Object hm) -> Just hm
  s -> Just $ fromList [(key, s)]

flatten :: Text -> Text -> Value -> Maybe Value
flatten key1 key2 = \case
  (Object v) -> do
    let tryFlatten k = lookup k v >>= flatten1 k
    (<>) <$> tryFlatten key1 <*> tryFlatten key2 <&> Object
  _ -> Nothing
