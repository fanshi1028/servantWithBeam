{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Util.Docs where

import Controllers.Home (HomeAPI)
import Data.Data (Proxy (Proxy))
import Data.Text (Text)
import Data.Time (LocalTime (LocalTime), midnight)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay))
import Database.Beam (Identity)
import Databases.HitmenBusiness.Handlers (HandlerB)
import Servant.Docs (ToSample (..), docs, singleSample)

-- instance ToSample LocalTime where
--   toSamples _ = singleSample $ LocalTime _ $ midnight

-- instance ToSample Text => ToSample (HandlerB Identity)

-- temp = docs @HomeAPI Proxy
