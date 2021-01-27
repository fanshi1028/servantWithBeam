{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Utils.Account.Auth (Login, authServer, AuthApi) where

import Control.Natural (type (~>))
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Password (PasswordCheck (PasswordCheckFail, PasswordCheckSuccess))
import Data.Validity (Validity)
import Database.Beam (Database, FromBackendRow, HasSqlEqualityCheck, Table (..), default_, insert, insertData, insertExpressions, runInsert, runSelectReturningOne, select, val_)
import Database.Beam.Backend (BeamSqlBackendCanSerialize)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning, runInsertReturningList)
import Databases.HitmenBusiness.Utils.Auth (getUserInfoWithPasswordHash)
import Databases.HitmenBusiness.Utils.Chronos (currentTimestamp_')
import Databases.HitmenBusiness.Utils.JSON (noCamelOpt)
import Databases.HitmenBusiness.Utils.Password (NewPassword (..), PasswordAlgorithm (..), WithNewPassword (WithNewPass), WithPassword (WithPass))
import Servant (Get, Handler, Header, Headers, JSON, NoContent (..), Post, ReqBody, ServerT, StdMethod (POST), Verb, err400, err401, errBody, throwError, (:<|>) ((:<|>)), (:>))
import Servant.Auth.Server (SetCookie, ToJWT (..), acceptLogin, clearSession)
import Universum
import Utils.Account.Login (LoginId, LoginT (..))
import Utils.Account.SignUp (Payload, SignUp, WithUserName (..), validateSignUp)
import Utils.Constraints (CreateBodyConstraint, ReadOneConstraint)
import Utils.Meta (WithMetaInfo, addMetaInfo)
import Utils.Types (MyServer, TableGetter)
import Validation (Validation (Failure, Success))

type Login userT = WithPassword $ LoginId userT

instance (FromJSON $ LoginId userT) => FromJSON (WithPassword $ LoginId userT) where
  parseJSON = genericParseJSON noCamelOpt

instance (ToJSON $ LoginId userT) => ToJSON (WithPassword $ LoginId userT) where
  toJSON = genericToJSON noCamelOpt
  toEncoding = genericToEncoding noCamelOpt

type AuthCookiesContent = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent

type AuthApi userT =
  ("signup" :> ReqBody '[JSON] (SignUp userT) :> Post '[JSON] NoContent)
    :<|> ("login" :> ReqBody '[JSON] (Login userT) :> Verb 'POST 204 '[JSON] AuthCookiesContent)
    :<|> ("logout" :> Get '[JSON] AuthCookiesContent)

authServer ::
  ( Database be db,
    With '[Typeable] userT,
    With '[Validity] (Payload userT),
    ReadOneConstraint be userT,
    CreateBodyConstraint be userT,
    Generic (userT Identity),
    With '[HasSqlEqualityCheck be, BeamSqlBackendCanSerialize be] (LoginId userT),
    ToJWT (WithMetaInfo userT Identity),
    With '[FromBackendRow be, BeamSqlBackendCanSerialize be] Text,
    With '[MonadBeamInsertReturning be] m,
    With '[Typeable, PasswordAlgorithm] crypto
  ) =>
  TableGetter be db (LoginT crypto userT) ->
  TableGetter be db (WithMetaInfo userT) ->
  (m ~> MyServer be db conn msg Handler) ->
  ServerT (AuthApi userT) (MyServer be db conn msg Handler)
authServer loginTableGetter userInfoTableGetter doQuery = signUp :<|> login :<|> logout
  where
    authCheck (WithPass pw userName) = do
      (loginTable, userInfoTable) <- (loginTableGetter &&& userInfoTableGetter) . view #_db <$> ask
      getUserInfoWithPasswordHash loginTable userInfoTable userName & select & runSelectReturningOne & doQuery >>= \case
        Nothing -> throwError err401
        Just (userInfo, hash) -> case checkPassword pw hash of
          PasswordCheckFail -> throwError err401
          PasswordCheckSuccess -> return userInfo
    login payload = do
      (cs, jwts) <- (view #_cs &&& view #_jwts) <$> ask
      authCheck payload
        >>= liftIO . acceptLogin cs jwts
        >>= maybe (throwError err401) (return . ($ NoContent))
    signUp su@(WithNewPass (NewPassword pw) _) = case validateSignUp su of
      Failure e -> throwError err400 {errBody = encodeUtf8 $ "Signup fail: \n" <> unlines (toList e)}
      Success (WithUserName name base) -> do
        hpw <- liftIO $ hashPassword pw
        (loginTable, userInfoTable) <- (loginTableGetter &&& userInfoTableGetter) . view #_db <$> ask
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
    logout = flip clearSession NoContent . view #_cs <$> ask
