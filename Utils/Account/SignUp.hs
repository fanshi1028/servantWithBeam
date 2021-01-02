{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Account.SignUp where

import Data.Aeson (FromJSON (..))
import Databases.HitmenBusiness.Utils.JSON (flatten, noCamelOpt)
import Databases.HitmenBusiness.Utils.Password (NewPassword (..), WithNewPassword (WithNewPass), validatePassword, zxcvbnStrength)
import Universum
import Utils.Account.Login (LoginId)
import Validation (Validation)

data WithUserName userT a = WithUserName (LoginId userT) a

instance (FromJSON a, FromJSON (LoginId userT)) => FromJSON (WithUserName userT a) where
  parseJSON ob = WithUserName <$> parseJSON ob <*> parseJSON ob

instance (ToJSON $ a, ToJSON $ LoginId userT) => ToJSON (WithUserName userT a) where
  toJSON = fromMaybe (String "JSON encode fail") . flatten "login" "content" <$> genericToJSON noCamelOpt

type Payload userT = WithUserName userT $ userT Identity

type SignUp userT = WithNewPassword $ Payload userT

class Validatable a where
  valiatePayload :: Payload a -> Validation e $ Payload a

-- valiatePayload = Success -- FIXME

validateSignUp :: Validatable userT => SignUp userT -> Validation (NonEmpty Text) $ Payload userT
validateSignUp (WithNewPass (NewPassword npw) payload) =
  const <$> valiatePayload payload
    <*> (validatePassword npw <> zxcvbnStrength npw)
