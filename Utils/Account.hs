{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Utils.Account (ProtectApi, LoginT, protected, protectedServer) where

import Control.Monad.Except (MonadError)
import Database.Beam (Database, DatabaseEntity, FromBackendRow, HasQBuilder, HasSqlEqualityCheck, PrimaryKey, Table (..), TableEntity)
import Database.Beam.Backend (BeamSqlBackendCanSerialize)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning)
import Database.Beam.Schema.Tables (FieldsFulfillConstraint)
import Databases.HitmenBusiness.Utils.Password (PasswordAlgorithm (..))
import Servant (Delete, Header, Headers, JSON, NoContent (..), Post, ReqBody, ServerError, ServerT, Verb, err401, (:<|>) ((:<|>)), (:>))
import Servant.Auth (Auth)
import Servant.Auth.Server (AuthResult (Authenticated), CookieSettings, JWTSettings, SetCookie, ThrowAll, ToJWT, throwAll)
import Typeclass.Meta (Meta, WithMetaInfo)
import Universum
import Utils.Account.Auth (Login, authServer)
import Utils.Account.Login (LoginId, LoginT (..))
import Utils.Account.SignUp (SignUp, Validatable)

type AuthCookiesContent = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent

type AuthApi userT =
  ("signup" :> ReqBody '[JSON] (SignUp userT) :> Post '[JSON] NoContent)
    :<|> ("login" :> ReqBody '[JSON] (Login userT) :> Verb Post 204 '[JSON] AuthCookiesContent)
    :<|> ("logout" :> Verb Delete 204 '[JSON] AuthCookiesContent)

type ProtectApi userT auths api = (Auth auths (userT Identity) :> api)

protected :: ThrowAll server => (userInfo -> server) -> AuthResult userInfo -> server
protected toServer = \case
  Authenticated userInfo -> toServer userInfo
  _ -> throwAll err401

protectedServer ::
  ( Database be db,
    HasQBuilder be,
    With '[Typeable, Meta be, Validatable] userT,
    With '[Table] (WithMetaInfo userT),
    With '[HasSqlEqualityCheck be, BeamSqlBackendCanSerialize be] (LoginId userT),
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) (PrimaryKey $ WithMetaInfo userT),
    FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey $ WithMetaInfo userT),
    With '[ToJWT, FromBackendRow be] (WithMetaInfo userT Identity),
    With '[Generic] (userT Identity),
    With '[FromBackendRow be, BeamSqlBackendCanSerialize be] Text,
    With '[MonadBeamInsertReturning be] m,
    With '[MonadIO, MonadError ServerError] n,
    With '[Typeable, PasswordAlgorithm] crypto,
    ThrowAll $ ServerT api n
  ) =>
  Proxy api ->
  (userT Identity -> ServerT api n) ->
  DatabaseEntity be db $ TableEntity $ LoginT crypto userT ->
  DatabaseEntity be db $ TableEntity $ WithMetaInfo userT ->
  (forall a. m a -> n a) ->
  CookieSettings ->
  JWTSettings ->
  ServerT (ProtectApi userT auths api :<|> AuthApi userT) n
protectedServer _ server loginTable userInfoTable doQuery cs jwts = protected server :<|> authServer loginTable userInfoTable doQuery cs jwts
