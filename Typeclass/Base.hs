{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Typeclass.Base (ToBase (..)) where

import Database.Beam (QAssignment, QField)
import Database.Beam.Query.Types (QExpr)
import Universum

class ToBase be a where
  type Base a :: (* -> *) -> *

  -- type Extra a :: *
  -- fromBase :: Base a Identity -> Extra a -> a (QExpr be s)
  fromBase :: Base a Identity -> a (QExpr be s)
  baseAsUpdate :: Base a Identity -> a (QField s) -> QAssignment be s

-- default baseAsUpdate ::
--   ( SqlValable (Base a Identity),
--     IsLabel "_base" (a (QField s) -> a (QExpr be s))
--   ) =>
--   Base a Identity ->
--   a (QField s) ->
--   QAssignment be s
-- baseAsUpdate body = (<-. val_ body) . #_base

-- toBase2 :: Base a (QField s) -> QAssignment be s

-- toBase :: forall f. a f -> Base a f

-- updateWithBase :: Base a Identity -> PrimaryKey a Identity -> (a (QField s) -> QAssignment be s)

-- fromBase :: Base a Identity -> Maybe (PrimaryKey a Identity) -> a (QExpr be s)

-- baseAsUpdate :: Base a Identity -> PrimaryKey a Identity -> _

-- fromBase :: Base a Identity -> a (QExpr be s)
-- fromBaseWithId :: Base a Identity -> PrimaryKey a Identity -> a (QExpr be s)

-- instance (BeamSqlBackend be, BeamSqlBackendCanSerialize be Text, BeamSqlBackendCanSerialize be UTCTime) => ToBase be HandlerT where
--   type Base HandlerT = HandlerB
--   fromBase b =
--     HandlerAll
--       { _handlerBase = val_ b,
--         _handlerId = default_,
--         _handlerCreatedAt = default_
--       }
--   fromBaseWithId b (HandlerId id) =
--     HandlerAll
--       { _handlerBase = val_ b,
--         _handlerId = val_ id,
--         _handlerCreatedAt = default_
--       }

-- fromBase b = HandlerAll default_ (val_ b) default_
