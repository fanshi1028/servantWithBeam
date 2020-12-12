{-# LANGUAGE OverloadedLabels #-}

module Controllers.Marks
  ( simpleCRUDServerForMarks,
  )
where

import Controllers.Utils (simpleCRUDServerForHitmenBusiness)

simpleCRUDServerForMarks = simpleCRUDServerForHitmenBusiness #_marks
