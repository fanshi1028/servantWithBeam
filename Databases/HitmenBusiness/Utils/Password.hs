{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Databases.HitmenBusiness.Utils.Password where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), genericToJSON, withObject, (.:))
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (typeMismatch)
import Data.Password (PasswordCheck, unsafeShowPassword)
import Data.Password.Argon2 (Argon2, Password, PasswordHash (..), Salt (..), defaultParams, hashPasswordWithSalt, mkPassword)
import qualified Data.Password.Argon2 as Argon2 (checkPassword, hashPassword)
import Data.Password.Validate (ValidationResult (..), defaultPasswordPolicy_)
import qualified Data.Password.Validate as PV (validatePassword)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDateValid)
import Database.Beam.Backend (BeamBackend, FromBackendRow, HasSqlValueSyntax (..))
import Databases.HitmenBusiness.Utils.JSON (flatten, noCamelOpt)
import Servant.Docs (ToSample (..), noSamples, singleSample, toSample)
import Text.Password.Strength (Strength (Safe, Strong), en_US, score, strength)
import qualified Text.Read as TR (get, prec, readPrec)
import Universum
import Validation (Validation (Failure, Success))

-- | Password
newtype NewPassword = NewPassword {unNewPassword :: Password} deriving newtype (ToSample, Show, ToJSON)

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
      _ -> fail "The new_password doesn't match with the confirm_password"

-- >>> validatePassword "fewh"
-- Failure ("PasswordTooShort 8 4" :| ["NotEnoughReqChars Uppercase 1 0","NotEnoughReqChars Digit 1 0"])
validatePassword :: (IsString s) => Password -> Validation (NonEmpty s) ()
validatePassword =
  PV.validatePassword defaultPasswordPolicy_ >>= \case
    ValidPassword -> return $ Success ()
    InvalidPassword reasons ->
      return $
        Failure $
          fromMaybe ("Impossible, password is invalid with unknown reasons" :| []) $ nonEmpty $ map show reasons

-- >>> zxcvbnStrength "fewh"
-- Failure ("{\"strength\":\"Weak\",\"score\":3600}" :| [])
-- zxcvbnStrength :: Password -> Validation (NonEmpty LByteString) ()
zxcvbnStrength :: Password -> Validation (NonEmpty LText) ()
zxcvbnStrength pw =
  maybe
    (Failure $ "Impossible, the zxcvbn checker got wrong ref day input!" :| [])
    ( \day ->
        let sc = score en_US day $ unsafeShowPassword pw
         in case strength sc of
              Strong -> pure ()
              Safe -> pure ()
              _ -> Failure $ encodeToLazyText sc :| []
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

-- instance HasColumnType (PasswordHash a)

-- >>> toSamples @(PasswordHash Argon2) Proxy
-- [("",PasswordHash {unPasswordHash = "$argon2id$v=19$m=65536,t=2,p=1$ZndlZ3dncmdnZ2dyd2V3a2s4MzQ3$tBE4ORs4fF4F7rfsUsY8kkyEi7uurt5cy3bTaHw83GM="})]
instance ToSample (PasswordHash Argon2) where
  toSamples _ = singleSample $ hashPasswordWithSalt defaultParams (Salt "fwegwgrggggrwewkk8347") $ mkPassword "jfwehfgwef"

data WithPassword a = WithPass
  { _password :: Password,
    _login :: a
  }
  deriving (Generic)

instance (ToSample a) => ToSample (WithPassword a) where
  toSamples _ = maybe noSamples singleSample $ WithPass <$> toSample Proxy <*> toSample Proxy

data WithNewPassword a = WithNewPass
  { _newPassword :: NewPassword,
    _newLogin :: a
  }
  deriving (Generic)

instance (ToSample a, ToSample NewPassword) => ToSample (WithNewPassword a) where
  toSamples _ = maybe noSamples singleSample $ WithNewPass <$> toSample Proxy <*> toSample Proxy

instance (FromJSON a) => FromJSON (WithNewPassword a) where
  parseJSON = withObject "Object with new_password and confirm_password" $
    \o -> WithNewPass <$> parseJSON (Object o) <*> parseJSON (Object o)

instance (ToJSON a) => ToJSON (WithNewPassword a) where
  toJSON = fromMaybe (String "Impossible! JSON encode WithNewPassword fail") . flatten "new_password" "new_login" <$> genericToJSON noCamelOpt

class PasswordAlgorithm crypto where
  checkPassword :: Password -> PasswordHash crypto -> PasswordCheck
  hashPassword :: Password -> IO $ PasswordHash crypto

instance PasswordAlgorithm Argon2 where
  checkPassword = Argon2.checkPassword
  hashPassword = Argon2.hashPassword
