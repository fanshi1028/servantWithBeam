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

temp name = do
  manager' <- newManager defaultManagerSettings
  -- hi <- runClientM (createHandler $ Handler "on9" Nothing) (mkClientEnv manager' (BaseUrl Http "localhost" 6868 ""))
  hi <- runClientM (createHandler $ Handler name Nothing) (mkClientEnv manager' (BaseUrl Http "localhost" 6868 ""))
  -- hi <- runClientM (deleteHandler $ HandlerId 2) (mkClientEnv manager' (BaseUrl Http "localhost" 6868 ""))
  case hi of
    Left e -> return $ "Error: " ++ show e
    Right _ -> do
      return "fuck you"

-- >>> temp "annnn"
-- "fuck you"
