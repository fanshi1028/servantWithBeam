{-# LANGUAGE OverloadedLabels #-}

module Controllers.Hitmen
  ( simpleCRUDServerForHitman,
  )
where

import Controllers.Utils (simpleCRUDServerForHitmenBusiness)

simpleCRUDServerForHitman = simpleCRUDServerForHitmenBusiness #_hitmen
