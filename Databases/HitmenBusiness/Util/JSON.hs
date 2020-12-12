{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Databases.HitmenBusiness.Util.JSON (noCamelOpt, flattenBase) where

import Data.Aeson (Options (fieldLabelModifier), Value (Object), camelTo2, defaultOptions)
import Data.Char (toUpper)
import Data.HashMap.Strict (delete, lookup)
import Data.Text (pack, split, splitOn, unpack)
import Prelude (fmap, foldMap, maybe, tail, ($), (.), (<$>), (<>), (==))

noCamelOpt = defaultOptions {fieldLabelModifier = camelTo2 '_' . tail}

flattenBase obj@(Object v) =
  maybe
    obj
    ( \case
        (Object base) -> Object $ delete "base" v <> base
        _ -> obj
    )
    (lookup "base" v)
