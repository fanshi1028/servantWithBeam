{-# LANGUAGE OverloadedLabels #-}

module Controllers.Handlers
  ( simpleCRUDServerForHandler,
  )
where

import Controllers.Utils (simpleCRUDServerForHitmenBusiness)

simpleCRUDServerForHandler = simpleCRUDServerForHitmenBusiness #_handlers
