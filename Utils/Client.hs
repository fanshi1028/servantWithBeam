{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Utils.Client where

import Data.Data (Proxy (Proxy))
import Databases.HitmenBusiness.Handlers (HandlerB (Handler))
import Databases.HitmenBusiness.Hitmen (HitmanB (Hitman))
import Databases.HitmenBusiness.Utils.Types (Codename (Codename))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant ((:<|>) ((:<|>)))
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http), client, mkClientEnv, runClientM)
import Servers (HomeAPI)
import Universum

(createHandler :<|> getHandlers :<|> getHandler :<|> updateHandler :<|> deleteHandler)
  :<|> (createHitman :<|> getHitmen :<|> getHitman :<|> updateHitman :<|> deleteHitman)
  :<|> (createMark :<|> getMarks :<|> getMark :<|> updateMark :<|> deleteMark)
  :<|> (createErasedMark :<|> getErasedMarks :<|> getErasedMark :<|> updateErasedMark :<|> deleteErasedMark)
  :<|> (createPursuingMark :<|> getPursuingMarks :<|> getPursuingMark :<|> updatePursuingMark :<|> deletePursuingMark) = client @HomeAPI Proxy

temp name = do
  manager' <- newManager defaultManagerSettings
  -- hi <- runClientM (createHandler $ Handler (Codename name) Nothing) (mkClientEnv manager' (BaseUrl Http "localhost" 6868 ""))
  hi <- runClientM (createHitman $ Hitman (Codename name) Nothing) (mkClientEnv manager' (BaseUrl Http "localhost" 6868 ""))
  -- hi <- runClientM (deleteHandler $ HandlerId 2) (mkClientEnv manager' (BaseUrl Http "localhost" 6868 ""))
  case hi of
    Left e -> return $ "Error: " ++ show e
    Right _ -> do
      return "fuck you"

-- >>> temp "yesman"
-- "Error: FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Http, baseUrlHost = \"localhost\", baseUrlPort = 6868, baseUrlPath = \"\"},\"/handlers\"), requestQueryString = fromList [], requestBody = Just ((),application/json;charset=utf-8), requestAccept = fromList [], requestHeaders = fromList []), requestHttpVersion = HTTP/1.1, requestMethod = \"POST\"} (Response {responseStatusCode = Status {statusCode = 500, statusMessage = \"Internal Server Error\"}, responseHeaders = fromList [(\"Transfer-Encoding\",\"chunked\"),(\"Date\",\"Sun, 06 Dec 2020 15:51:20 GMT\"),(\"Server\",\"Warp/3.3.13\"),(\"Content-Type\",\"text/plain; charset=utf-8\")], responseHttpVersion = HTTP/1.1, responseBody = \"Exception: SqlError {sqlState = \\\"23505\\\", sqlExecStatus = FatalError, sqlErrorMsg = \\\"duplicate key value violates unique constraint \\\\\\\"handlers_codename_ukey\\\\\\\"\\\", sqlErrorDetail = \\\"Key (codename)=(yesman) already exists.\\\", sqlErrorHint = \\\"\\\"}\"})"
