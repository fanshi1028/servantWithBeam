{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Util.Client where

import Controllers.Home (HomeAPI)
import Data.Data (Proxy (Proxy))
import Databases.HitmenBusiness.Handlers (HandlerB (Handler))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant ((:<|>) ((:<|>)))
import Servant.Client (Scheme (Http), client, mkClientEnv, runClientM)
import Servant.Client.Streaming (BaseUrl (BaseUrl))

(createHandler :<|> getHandler :<|> getHandlers :<|> updateHandler :<|> deleteHandler)
  :<|> (createHitman :<|> getHitman :<|> getHitmen :<|> updateHitman :<|> deleteHitman)
  :<|> (createMark :<|> getMark :<|> getMarks :<|> updateMark :<|> deleteMark)
  :<|> (createErasedMark :<|> getErasedMark :<|> getErasedMarks :<|> updateErasedMark :<|> deleteErasedMark)
  :<|> (createPursuingMark :<|> getPursuingMark :<|> getPursuingMarks :<|> updatePursuingMark :<|> deletePursuingMark) = client @HomeAPI Proxy

temp = do
  manager' <- newManager defaultManagerSettings
  -- hi <- runClientM (createHandler $ Handler "on9" Nothing) (mkClientEnv manager' (BaseUrl Http "localhost" 6868 ""))
  hi <- runClientM (createHandler $ Handler "on99" Nothing) (mkClientEnv manager' (BaseUrl Http "localhost" 6868 ""))
  -- hi <- runClientM (deleteHandler $ HandlerId 2) (mkClientEnv manager' (BaseUrl Http "localhost" 6868 ""))
  case hi of
    Left e -> return $ "Error: " ++ show e
    Right _ -> do
      return "fuck you"

-- >>> temp
-- "Error: FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Http, baseUrlHost = \"localhost\", baseUrlPort = 6868, baseUrlPath = \"\"},\"/handlers\"), requestQueryString = fromList [], requestBody = Just ((),application/json;charset=utf-8), requestAccept = fromList [], requestHeaders = fromList []), requestHttpVersion = HTTP/1.1, requestMethod = \"POST\"} (Response {responseStatusCode = Status {statusCode = 500, statusMessage = \"Internal Server Error\"}, responseHeaders = fromList [(\"Transfer-Encoding\",\"chunked\"),(\"Date\",\"Fri, 04 Dec 2020 15:48:05 GMT\"),(\"Server\",\"Warp/3.3.13\"),(\"Content-Type\",\"text/plain; charset=utf-8\")], responseHttpVersion = HTTP/1.1, responseBody = \"Exception: SqlError {sqlState = \\\"23505\\\", sqlExecStatus = FatalError, sqlErrorMsg = \\\"duplicate key value violates unique constraint \\\\\\\"handlers_codename_ukey\\\\\\\"\\\", sqlErrorDetail = \\\"Key (codename)=(on99) already exists.\\\", sqlErrorHint = \\\"\\\"}\"})"
