{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Account.SignUp where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON)
import Data.Aeson.Types (genericToJSON)
import Data.Validity (Validity (validate), checkValidity, prettyValidate)
import Databases.HitmenBusiness.Utils.JSON (noCamelOpt)
import Databases.HitmenBusiness.Utils.Password (NewPassword (..), WithNewPassword (WithNewPass), validatePassword, zxcvbnStrength)
import Servant.Docs (ToSample (..), noSamples, singleSample, toSample)
import Universum
import Utils.Account.Login (LoginId)
import Validation (Validation (Failure, Success))

data WithUserName userT a = WithUserName
  { _userName :: LoginId userT,
    _payload :: a
  }
  deriving (Generic)

instance (ToSample a, ToSample $ LoginId userT) => ToSample (WithUserName userT a) where
  toSamples _ = maybe noSamples singleSample $ WithUserName <$> toSample (Proxy :: Proxy $ LoginId userT) <*> toSample Proxy

instance (FromJSON a, FromJSON $ LoginId userT) => FromJSON (WithUserName userT a) where
  parseJSON = genericParseJSON noCamelOpt

instance (ToJSON $ a, ToJSON $ LoginId userT) => ToJSON (WithUserName userT a) where
  toJSON = genericToJSON noCamelOpt

type Payload userT = WithUserName userT $ userT Identity

type SignUp userT = WithNewPassword $ Payload userT

validateSignUp :: Validity $ Payload userT => SignUp userT -> Validation (NonEmpty Text) $ Payload userT
validateSignUp (WithNewPass (NewPassword npw) payload) =
  const <$> valiatePayload payload
    <*> (validatePassword npw <> zxcvbnStrength npw)
  where
    valiatePayload = either (Failure . (:| []) . toText) Success . prettyValidate
