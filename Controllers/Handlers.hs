{-# LANGUAGE OverloadedLabels #-}

module Controllers.Handlers
  ( simpleCRUDServerForHandler,
  )
where

import Controllers.Util (simpleCRUDServerForHitmenBusiness)

simpleCRUDServerForHandler = simpleCRUDServerForHitmenBusiness #_handlers
