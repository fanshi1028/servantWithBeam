{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Databases.HitmenBusiness.Utils.Password where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withObject, (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Password (PasswordCheck)
import Data.Password.Argon2 (Argon2, Password, PasswordHash (..), Salt (..), defaultParams, hashPasswordWithSalt, mkPassword)
import qualified Data.Password.Argon2 as Argon2 (checkPassword, hashPassword)
import Data.Password.Validate (ValidationResult (..), defaultPasswordPolicy_)
import qualified Data.Password.Validate as PV (validatePassword)
import Database.Beam.AutoMigrate (HasColumnType)
import Database.Beam.Backend (BeamBackend, FromBackendRow, HasSqlValueSyntax (..))
import Servant.Docs (ToSample (..), singleSample)
import qualified Text.Read as TR (get, prec, readPrec)
import Universum
import Validation (Validation (Failure, Success))

-- | Password
newtype NewPassword = NewPassword {unNewPassword :: Password} deriving (Show)

-- >>> import qualified Data.Aeson as A
-- >>> A.eitherDecode @NewPassword "{ \"new_password\": \"jfwoiefjwef\", \"confirm_password\":\"fweew\"}"
-- Left "Error in $: New password doesn't match with the confirm password"

-- >>> import qualified Data.Aeson as A
-- >>> A.eitherDecode @NewPassword "{ \"new_password\": \"jfwoiefjwef\", \"confirm_password\":\"jfwoiefjwef\"}"
-- Right (NewPassword {unNewPassword = **PASSWORD**})
instance FromJSON NewPassword where
  parseJSON = withObject "NewPassword" $ \o -> do
    np <- o .: "new_password"
    o .: "confirm_password" >>= \case
      cp | cp == np -> return $ NewPassword $ mkPassword np
      _ -> fail "New password doesn't match with the confirm password"

validatePassword :: Password -> Validation (NonEmpty Text) Password
validatePassword =
  PV.validatePassword defaultPasswordPolicy_ >>= \case
    ValidPassword -> return $ Success $ mkPassword ""
    InvalidPassword reasons -> return $ Failure $ fromMaybe ("Impossible, password is invalid with unknown reasons" :| []) $ nonEmpty $ map show reasons

-- >>> readEither @Text @Password "fwefewfwfw1efe"
-- Left "Prelude.read: no parse"
instance Read Password where
  readPrec = TR.prec 0 $ do
    s <- many TR.get
    let pw = mkPassword $ toText s
    case PV.validatePassword defaultPasswordPolicy_ pw of
      ValidPassword -> return pw
      InvalidPassword (e : es) -> fail $ show e

instance ToSample Password where
  toSamples _ = singleSample $ mkPassword "123456789"

-- >>> toJSON $ mkPassword "jfwoiefjwef"
-- String "**PASSWORD**"
instance ToJSON Password where
  toJSON = toJSON . show @Text

-- >>> import qualified Data.Aeson as A
-- >>> A.eitherDecode @Password "\"jfwoiefjwef\""
-- Right **PASSWORD**
instance FromJSON Password where
  parseJSON = \case
    (String s) -> return $ mkPassword s
    v -> typeMismatch "Password are string" v

deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be (PasswordHash a)

deriving newtype instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be (PasswordHash a)

deriving newtype instance HasColumnType (PasswordHash a)

-- >>> toSamples @(PasswordHash Argon2) Proxy
-- [("",PasswordHash {unPasswordHash = "$argon2id$v=19$m=65536,t=2,p=1$ZndlZ3dncmdnZ2dyd2V3a2s4MzQ3$tBE4ORs4fF4F7rfsUsY8kkyEi7uurt5cy3bTaHw83GM="})]
instance ToSample (PasswordHash Argon2) where
  toSamples _ = singleSample $ hashPasswordWithSalt defaultParams (Salt "fwegwgrggggrwewkk8347") $ mkPassword "jfwehfgwef"

data WithPassword a = WithPass Password a

data WithNewPassword a = WithNewPass NewPassword a

instance (FromJSON a) => FromJSON (WithPassword a) where
  parseJSON ob@(Object o) = WithPass <$> o .: "password" <*> parseJSON ob

instance (FromJSON a) => FromJSON (WithNewPassword a) where
  parseJSON ob = WithNewPass <$> parseJSON ob <*> parseJSON ob

-- class (Typeable crypto) => PasswordAlgorithm crypto where
class PasswordAlgorithm crypto where
  checkPassword :: Password -> PasswordHash crypto -> PasswordCheck
  hashPassword :: Password -> IO $ PasswordHash crypto

instance PasswordAlgorithm Argon2 where
  checkPassword = Argon2.checkPassword
  hashPassword = Argon2.hashPassword
