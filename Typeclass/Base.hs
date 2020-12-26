{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Typeclass.Base (ToBase (..)) where

import Database.Beam (QAssignment, QField)
import Database.Beam.Query.Types (QExpr)
import Universum

class ToBase be a where
  type Base a :: (* -> *) -> *
  fromBase :: Base a Identity -> a (QExpr be s)
  baseAsUpdate :: Base a Identity -> a (QField s) -> QAssignment be s
