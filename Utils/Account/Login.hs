{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Account.Login (LoginT (..), LoginId) where

import Chronos (Datetime)
import Data.Password (PasswordHash (..))
import Database.Beam (Beamable, C, PrimaryKey, Table (..))
import Database.Beam.Backend (SqlSerial (..))
import Universum
import Utils.Meta (WithMetaInfo)

type family LoginId (userT :: (* -> *) -> *)

data LoginT crypto userT f = LoginAccount
  { _accountId :: C f (SqlSerial Int32),
    _account :: PrimaryKey (WithMetaInfo userT) f,
    _accountName :: C f (LoginId userT),
    _passwordHash :: C f (PasswordHash crypto),
    _createdAt :: C f Datetime
  }
  deriving (Generic)

deriving instance Beamable $ PrimaryKey $ WithMetaInfo userT => Beamable (LoginT crypto userT)

instance (Typeable crypto, Typeable userT, Beamable $ PrimaryKey $ WithMetaInfo userT) => Table (LoginT crypto userT) where
  data PrimaryKey (LoginT crypto userT) f = UserId (C f (SqlSerial Int32)) deriving (Generic, Beamable)
  primaryKey = UserId . _accountId
