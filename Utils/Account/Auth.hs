{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Utils.Account.Auth (Login, authServer, AuthApi) where

import Control.Monad.Except (MonadError)
import Data.Password (PasswordCheck (PasswordCheckFail, PasswordCheckSuccess))
import Database.Beam (Database, DatabaseEntity, FromBackendRow, HasQBuilder, HasSqlEqualityCheck, PrimaryKey, Table (..), TableEntity, default_, insert, insertData, insertExpressions, runInsert, runSelectReturningOne, select, val_)
import Database.Beam.Backend (BeamSqlBackendCanSerialize)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning, runInsertReturningList)
import Database.Beam.Schema.Tables (FieldsFulfillConstraint)
import Databases.HitmenBusiness.Utils.Auth (getUserInfoWithPasswordHash)
import Databases.HitmenBusiness.Utils.Chronos (currentTimestamp_')
import Databases.HitmenBusiness.Utils.Password (NewPassword (..), PasswordAlgorithm (..), WithNewPassword (WithNewPass), WithPassword (WithPass))
import Servant (Header, Headers, JSON, NoContent (..), Post, ReqBody, ServerError, ServerT, StdMethod (DELETE, POST), Verb, err400, err401, errBody, throwError, (:<|>) ((:<|>)), (:>))
import Servant.Auth.Server (CookieSettings, JWTSettings, SetCookie, ToJWT (..), acceptLogin, clearSession)
import Universum
import Utils.Account.Login (LoginId, LoginT (..))
import Utils.Account.SignUp (SignUp, Validatable, WithUserName (..), validateSignUp)
import Utils.Meta (Meta, WithMetaInfo, addMetaInfo)
import Validation (Validation (Failure, Success))

type Login userT = WithPassword $ LoginId userT

type AuthCookiesContent = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent

type AuthApi userT =
  ("signup" :> ReqBody '[JSON] (SignUp userT) :> Post '[JSON] NoContent)
    :<|> ("login" :> ReqBody '[JSON] (Login userT) :> Verb 'POST 204 '[JSON] AuthCookiesContent)
    :<|> ("logout" :> Verb 'DELETE 204 '[JSON] AuthCookiesContent) -- FIXME logout route, not DELTE right?

authServer ::
  ( Database be db,
    HasQBuilder be,
    With '[Typeable, Meta be, Validatable] userT,
    With '[Table] (WithMetaInfo userT),
    With '[HasSqlEqualityCheck be, BeamSqlBackendCanSerialize be] (LoginId userT),
    FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) (PrimaryKey $ WithMetaInfo userT),
    FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey $ WithMetaInfo userT),
    With '[Generic] (userT Identity),
    With '[ToJWT, FromBackendRow be] (WithMetaInfo userT Identity),
    With '[FromBackendRow be, BeamSqlBackendCanSerialize be] Text,
    With '[MonadBeamInsertReturning be] m,
    With '[MonadIO, MonadError ServerError] n,
    With '[Typeable, PasswordAlgorithm] crypto
  ) =>
  DatabaseEntity be db $ TableEntity $ LoginT crypto userT ->
  DatabaseEntity be db $ TableEntity $ WithMetaInfo userT ->
  (forall a. m a -> n a) ->
  CookieSettings ->
  JWTSettings ->
  ServerT (AuthApi userT) n
authServer loginTable userInfoTable doQuery cs jwts = signUp :<|> login :<|> logout
  where
    authCheck (WithPass pw userName) =
      getUserInfoWithPasswordHash loginTable userInfoTable userName & select & runSelectReturningOne & doQuery >>= \case
        Nothing -> throwError err401
        Just (userInfo, hash) -> case checkPassword pw hash of
          PasswordCheckFail -> throwError err401
          PasswordCheckSuccess -> return userInfo
    login payload =
      authCheck payload
        >>= liftIO . acceptLogin cs jwts
        >>= maybe (throwError err401) (return . ($ NoContent))
    signUp su@(WithNewPass (NewPassword pw) _) = case validateSignUp su of
      Failure e -> throwError err400 {errBody = show e}
      Success (WithUserName name base) -> do
        hpw <- liftIO $ hashPassword pw
        let insertUserTable = runInsertReturningList $ insert userInfoTable $ insertExpressions [addMetaInfo base]
            mkLoginExpression user =
              LoginAccount
                { _accountId = default_,
                  _account = val_ $ primaryKey user,
                  _accountName = val_ name,
                  _passwordHash = val_ hpw,
                  _createdAt = currentTimestamp_'
                }
            goSignUp = insertUserTable >>= runInsert . insert loginTable . insertData . map mkLoginExpression
        doQuery goSignUp >> return NoContent
    logout = return $ clearSession cs NoContent
