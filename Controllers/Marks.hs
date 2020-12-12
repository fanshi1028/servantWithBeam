{-# LANGUAGE OverloadedLabels #-}

module Controllers.Marks
  ( simpleCRUDServerForMarks,
  )
where

import Controllers.Util (simpleCRUDServerForHitmenBusiness)

simpleCRUDServerForMarks = simpleCRUDServerForHitmenBusiness #_marks
