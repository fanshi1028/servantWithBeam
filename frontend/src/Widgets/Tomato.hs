{-# LANGUAGE FlexibleContexts #-}

module Widgets.Tomato (tomato) where

import Control.Monad.Fix (MonadFix)
import Data.Time (addUTCTime, getCurrentTime, midnight)
import Data.Time.Compat (secondsToNominalDiffTime)
import Reflex.Dom hiding (now, (.~))
import Universum
import Utils.Components (dangerCard, dangerMessage, navbar, primaryMessage)

data TomatoState = Working Int | ShortBreak Int | LongBreak Int

clockWidget :: (MonadIO m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), PostBuild t m, MonadHold t m, MonadFix m) => m (Dynamic t TickInfo)
clockWidget = do
  t <- liftIO getCurrentTime
  clockLossy (secondsToNominalDiffTime 1) t

tomato :: (DomBuilder t m, MonadIO m, MonadIO (Performable m), MonadFix m, MonadHold t m, PostBuild t m, TriggerEvent t m, PerformEvent t m) => m ()
tomato = divClass "block" $ do
  navbar "Tomato" "clock-o"
  divClass "box" $
    elClass "span" "tags has-addons" $ do
      elClass "span" "tag" $ text "hi"
      elClass "span" "tag is-danger" $ text "hi3"
      elClass "span" "tag is-primary" $ text "hi2"
      elClass "span" "tag is-danger" $ text "hi3"
  clock <- clockWidget
  let showTick = show . secondsToNominalDiffTime . (15 * 60 -) . fromIntegral . _tickInfo_n
  dangerMessage "Clock" $ dynText $ showTick <$> clock
  -- divClass "container" $
  divClass "columns is-multiline is-centered " $ do
    replicateM_ 3 $ divClass "column is-4" $ dangerCard "Clock" $ dynText $ showTick <$> clock
    -- divClass "column is-two-fifths" $ dangerCard "Clock" $ dynText $ showTick <$> clock
    -- divClass "column" $ dangerCard "Clock" $ dynText $ showTick <$> clock
    elAttr "div" ("class" =: "column" <> "style" =: "width: 38.1971%; flex: none")  $ dangerCard "Clock" $ dynText $ showTick <$> clock
    divClass "column" $ blank
