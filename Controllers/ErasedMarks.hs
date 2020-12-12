{-# LANGUAGE OverloadedLabels #-}

module Controllers.ErasedMarks
  ( simpleCRUDServerForErasedMarks,
  )
where

import Controllers.Util (simpleCRUDServerForHitmenBusiness)

simpleCRUDServerForErasedMarks = simpleCRUDServerForHitmenBusiness #_hbErasedMarks
