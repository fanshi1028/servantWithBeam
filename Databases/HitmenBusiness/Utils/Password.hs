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
import Data.Time.Calendar.OrdinalDate (fromOrdinalDateValid)
import Database.Beam.AutoMigrate (HasColumnType)
import Database.Beam.Backend (BeamBackend, FromBackendRow, HasSqlValueSyntax (..))
import Servant.Docs (ToSample (..), singleSample)
import Text.Password.Strength (Strength (Safe, Strong), en_US, score, strength)
import qualified Text.Read as TR (get, prec, readPrec)
import Universum
import Validation (Validation (Failure, Success))
import Data.Password (unsafeShowPassword)

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

validatePassword :: Password -> Validation (NonEmpty Text) ()
validatePassword =
  PV.validatePassword defaultPasswordPolicy_ >>= \case
    ValidPassword -> return $ Success ()
    InvalidPassword reasons ->
      return $
        Failure $
          fromMaybe ("Impossible, password is invalid with unknown reasons" :| []) $ nonEmpty $ map show reasons

-- >>> zxcvbnStrength "fewhfiewfjowjfowwj"
-- Failure ("Object (fromList [(\"strength\",String \"Risky\"),(\"score\",Number 10.0)])" :| [])
zxcvbnStrength :: Password -> Validation (NonEmpty Text) ()
zxcvbnStrength pw =
  maybe
    (Failure $ "Impossible, the zxcvbn checker got wrong ref day input!" :| [])
    ( \day ->
        let sc = score en_US day $ unsafeShowPassword pw
         in case strength sc of
              Strong -> pure ()
              Safe -> pure ()
              _ -> Failure $ show (toJSON sc) :| []
    )
    $ fromOrdinalDateValid 2020 360

-- >>> readEither @Text @Password "fwefewfwfw1efe"
-- Right **PASSWORD**
instance Read Password where
  readPrec = TR.prec 0 $ do
    s <- many TR.get
    return $ mkPassword $ toText s

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
