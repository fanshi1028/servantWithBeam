{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Utils.Account (ProtectApi, LoginT, protected) where

import Servant (err401, (:>))
import Servant.Auth (Auth)
import Servant.Auth.Server (ThrowAll(..), AuthResult (Authenticated))
import Universum
import Utils.Account.Login (LoginT (..))
import Utils.Meta (WithMetaInfo)

type ProtectApi (auths :: [*]) userT api = Auth auths (WithMetaInfo userT Identity) :> api

-- protected :: ThrowAll server => (userInfo -> server) -> AuthResult userInfo -> server
protected :: (userInfo -> server) -> AuthResult userInfo -> server
-- protected :: (userInfo -> ServerT api m) -> AuthResult userInfo -> ServerT api m
protected toServer = \case
  Authenticated userInfo -> toServer userInfo
  -- _ -> throwAll err401
