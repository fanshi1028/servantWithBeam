{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Utils.Account (ProtectApi, LoginT, protected) where

import Servant (err401, (:>))
import Servant.Auth (Auth)
import Servant.Auth.Server (AuthResult (Authenticated), ThrowAll (..))
import Universum
import Utils.Account.Login (LoginT (..))
import Utils.Meta (WithMetaInfo)

type ProtectApi (auths :: [*]) userT api = Auth auths (WithMetaInfo userT Identity) :> api

protected :: ThrowAll server => (userInfo -> server) -> AuthResult userInfo -> server
protected toServer = \case
  Authenticated userInfo -> toServer userInfo
  _ -> throwAll err401
