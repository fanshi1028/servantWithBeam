{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Utils.Constraints where

import Database.Beam (Beamable, FromBackendRow, HasQBuilder, HasSqlEqualityCheck, PrimaryKey, Table)
import Database.Beam.Backend (BeamSqlBackendCanSerialize)
import Database.Beam.Schema.Tables (FieldsFulfillConstraint)
import Universum
import Utils.Meta (Meta, WithMetaInfo)

type QueryIdConstraint be a =
  ( FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) (PrimaryKey (WithMetaInfo a)),
    FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey (WithMetaInfo a))
  )

type CreateBodyConstraint be a =
  ( HasQBuilder be,
    Meta be a,
    Table (WithMetaInfo a)
  )

type ReadAllConstraint be a =
  ( HasQBuilder be,
    Table (WithMetaInfo a),
    FromBackendRow be (WithMetaInfo a Identity)
  )

type ReadOneConstraint be a =
  ( ReadAllConstraint be a,
    QueryIdConstraint be a
  )

type UpdateBodyConstraint be a =
  ( Beamable a,
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) a,
    CreateBodyConstraint be a,
    QueryIdConstraint be a
  )

type DeleteOneConstraint be a =
  ( HasQBuilder be,
    Table (WithMetaInfo a),
    QueryIdConstraint be a
  )

