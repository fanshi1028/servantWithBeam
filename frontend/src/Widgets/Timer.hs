{-# LANGUAGE DataKinds #-}
module Widgets.Timer (timer, TimerEvent) where

import Reflex.Dom hiding (Reset)
import Universum hiding (Element)

data TimerEvent = Start | Stop | Reset deriving (Show, Bounded)

-- timerButton :: DomBuilder t m => TimerEvent -> m TimerEvent
-- timerButton te = (te <$) <$> divClass "card-footer-item button" $ text $ show te

timerButton :: DomBuilder t m => TimerEvent -> m (Event t TimerEvent)
timerButton te = (te <$) . domEvent Click . fst <$> elClass' "button" "card-footer-item button" (text $ show te)

timer :: DomBuilder t m => m (Event t TimerEvent)
timer = divClass "card" $ do
  divClass "card-header is-flex-wrap-wrap" $ do
    divClass "card-header-title" $ text "Timer"
    divClass "card-header-icon" $ elClass "span" "icon is-centered" $ elClass "i" "fa fa-clock" blank
  divClass "card-content" $ text "hi"
  divClass "card-image" $ elAttr "img" ("src" =: "https://www.gardeningknowhow.com/wp-content/uploads/2020/04/Tomatoes.jpg") blank
  te <- divClass "card-footer is-flex-wrap-wrap" $ do
    mapM timerButton [Start, Stop, Reset]
  return $ leftmost te
