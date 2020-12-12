{-# LANGUAGE OverloadedLabels #-}

module Controllers.PursuingMarks
  ( simpleCRUDServerForPursuingMarks,
  )
where

import Controllers.Util (simpleCRUDServerForHitmenBusiness)

simpleCRUDServerForPursuingMarks = simpleCRUDServerForHitmenBusiness #_hbPursuingMarks
