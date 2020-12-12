{-# LANGUAGE OverloadedLabels #-}

module Controllers.Hitmen
  ( simpleCRUDServerForHitman,
  )
where

import Controllers.Util (simpleCRUDServerForHitmenBusiness)

simpleCRUDServerForHitman = simpleCRUDServerForHitmenBusiness #_hitmen
