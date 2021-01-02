{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Meta (Meta (..), WithMetaInfo (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), genericToJSON)
import Database.Beam (Beamable, QAssignment, QField, val_, (<-.))
import Database.Beam.Backend (BeamSqlBackend, BeamSqlBackendCanSerialize)
import Database.Beam.Query.Types (QExpr)
import Database.Beam.Schema.Tables (FieldsFulfillConstraint)
import Databases.HitmenBusiness.Utils.JSON (flatten, noCamelOpt)
import Servant.Docs (ToSample)
import Universum

class Meta be a where
  data MetaInfo a :: (* -> *) -> *
  addMetaInfo :: a Identity -> WithMetaInfo a (QExpr be s)
  updateWithMetaInfo ::
    (Beamable a, BeamSqlBackend be, FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) a) =>
    a Identity ->
    WithMetaInfo a (QField s) ->
    QAssignment be s
  updateWithMetaInfo body = (<-. val_ body) . _base

data WithMetaInfo a f = WithMetaInfo {_base :: a f, _metaInfo :: MetaInfo a f} deriving (Generic)

instance (ToJSON $ a f, ToJSON $ MetaInfo a f) => ToJSON (WithMetaInfo a f) where
  toJSON = fromMaybe (String "JSON encode fail") . flatten "base" "meta_info" <$> genericToJSON noCamelOpt

-- NOTE toEncoding seems harder to write as the builders are not very composable when amending the JSON structure
-- toEncoding = genericToEncoding noCamelOpt

instance (FromJSON $ a f, FromJSON $ MetaInfo a f) => FromJSON (WithMetaInfo a f) where
  parseJSON o = WithMetaInfo <$> parseJSON o <*> parseJSON o

deriving instance (Show (a f), Show (MetaInfo a f)) => Show (WithMetaInfo a f)

deriving instance (Beamable a, Beamable (MetaInfo a)) => Beamable (WithMetaInfo a)

deriving instance (ToSample (a f), ToSample (MetaInfo a f)) => ToSample (WithMetaInfo a f)
