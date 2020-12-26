{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Databases.HitmenBusiness.Utils.Auth (getUserInfoWithPasswordHash) where

import Data.Generics.Labels ()
import Data.Password (PasswordHash (..))
import Database.Beam (Database, DatabaseEntity, HasSqlEqualityCheck, PrimaryKey, Q, QExpr, Table (..), TableEntity, all_, filter_, oneToOne_, val_, (==.))
import Database.Beam.Backend (BeamSqlBackend, BeamSqlBackendCanSerialize)
import Database.Beam.Schema.Tables (FieldsFulfillConstraint)
import Models.HitmenBusiness (myJoin)
import Universum
import Utils.Account.Login (LoginId, LoginT)

joinAuth ::
  ( BeamSqlBackend be,
    Database be db,
    Table userT,
    FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey userT),
    Typeable crypto
  ) =>
  DatabaseEntity be db $ TableEntity $ LoginT crypto userT ->
  userT $ QExpr be s ->
  Q be db s $ LoginT crypto userT $ QExpr be s
joinAuth loginTable = oneToOne_ loginTable (view #_account)

getUserInfoWithPasswordHash ::
  ( BeamSqlBackend be,
    Database be db,
    Table userT,
    With '[BeamSqlBackendCanSerialize be, HasSqlEqualityCheck be] (LoginId userT),
    FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey userT),
    Generic $ userT Identity,
    Typeable crypto
  ) =>
  DatabaseEntity be db $ TableEntity $ LoginT crypto userT ->
  DatabaseEntity be db $ TableEntity $ userT ->
  LoginId userT ->
  Q be db s (userT $ QExpr be s, QExpr be s $ PasswordHash crypto)
getUserInfoWithPasswordHash loginTable userTable userName =
  -- bimap (view #_base) (view #_passwordHash) FIXME generic lens can't be derived for #_base
    second (view #_passwordHash)
    <$> filter_
      ((==. val_ userName) . view #_accountName . snd)
      (all_ userTable >>= myJoin (joinAuth loginTable))
