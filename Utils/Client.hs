{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Utils.Client where

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
  let runClientM' = flip runClientM $ mkClientEnv manager' (BaseUrl Http "localhost" 6868 "")
  -- runClientM' (createHandler $ Handler (Codename name) Nothing) >>= \case
  -- runClientM' (createHitman $ Hitman (Codename name) Nothing) >>= \case
  -- runClientM' (deleteHandler $ HandlerId 2) >>= \case
  runClientM' getHandlers >>= \case
    Left e -> return $ "Error: " ++ show e
    Right a -> do
      return $ show a

-- >>> temp "yesman"
-- "[WithMetaInfo {_base = Handler {_codename = \"yesman\", _dieAt = Nothing}, _metaInfo = HandlerMetaInfo {_handlerId = SqlSerial {unSerial = 1}, _createdAt = Datetime {datetimeDate = Date {dateYear = Year {getYear = 2020}, dateMonth = Month {getMonth = 11}, dateDay = DayOfMonth {getDayOfMonth = 12}}, datetimeTime = TimeOfDay {timeOfDayHour = 19, timeOfDayMinute = 16, timeOfDayNanoseconds = 46113930000}}}}]"
