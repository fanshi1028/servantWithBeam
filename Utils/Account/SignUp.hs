{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Account.SignUp where

import Data.Aeson (FromJSON (..))
import Databases.HitmenBusiness.Utils.Password (NewPassword (..), WithNewPassword (WithNewPass), validatePassword, zxcvbnStrength)
import Typeclass.Base (ToBase (Base))
import Universum
import Utils.Account.Login (LoginId)
import Validation (Validation (Success))

data WithUserName userT a = WithUserName (LoginId userT) a

instance (FromJSON a, FromJSON (LoginId userT)) => FromJSON (WithUserName userT a) where
  parseJSON ob = WithUserName <$> parseJSON ob <*> parseJSON ob

type Payload userT = WithUserName userT $ Base userT Identity

type SignUp userT = WithNewPassword $ Payload userT

class Validatable a where
  valiatePayload :: Payload a -> Validation e $ Payload a

-- valiatePayload = Success -- FIXME

validateSignUp :: Validatable userT => SignUp userT -> Validation (NonEmpty Text) $ Payload userT
validateSignUp (WithNewPass (NewPassword npw) payload) =
  const <$> valiatePayload payload
    <*> (validatePassword npw <> zxcvbnStrength npw)
