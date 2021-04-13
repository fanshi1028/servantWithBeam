{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Widgets.Tomato (tomato) where

import Control.Monad.Fix (MonadFix)
import qualified Data.HashMap.Internal as HM
import Data.HashSet (fromList, singleton)
import qualified Data.HashSet as HS
import Data.Time (addUTCTime, getCurrentTime, midnight)
import Data.Time.Compat (UTCTime, secondsToNominalDiffTime)
import Reflex.Dom hiding (button, now, tag, (.~))
import Universum
import Utils.Bulma.Components.Breadcrumb (breadcrumb)
import Utils.Bulma.Components.Card (cardClass, cardHeader)
import Utils.Bulma.Components.Modal (modal, modalCard)
import Utils.Bulma.Elements
import Utils.Components (dangerCard, dangerMessage, navbar, primaryMessage)
import Widgets.Timer (timer)

data TomatoState = Working Int | ShortBreak Int | LongBreak Int

data Position = Up | Center | Bottom deriving (Generic, Hashable, Eq, Ord, Show)

tomato :: (DomBuilder t m, MonadIO m, MonadIO (Performable m), MonadFix m, MonadHold t m, PostBuild t m, TriggerEvent t m, PerformEvent t m) => m ()
tomato = divClass "block" $ do
  navbar "Tomato" "clock-o"
  box $ do
    mkDeletableTag $ tagClass (singleton "is-primary") $ text "nino"
    mkDeletableTag $ do
      tag $ text "hi"
      tagClass (singleton "is-danger") $ text "hi3"
      tagClass (singleton "is-primary") $ text "hi2"
      tagClass (singleton "is-danger") $ text "hi3"
  ready <- getPostBuild >>= delay 0.5
  -- divClass "container is-fluid" $ columns $ do
  divClass "container is-fluid columns" $ do
    columnClass (singleton "is-9") $ do
      -- let pos = [(Up, "https://bulma.io/documentation/"), (Center, "https://bulma.io/documentation/components/"), (Bottom, "https://bulma.io/documentation/components/card/")]
      let pos = [(Up, "https://bulma.io/documentation/"), (Bottom, "https://bulma.io/documentation/components/card/")]
      block $ breadcrumb (HM.fromList pos) [Up, Center, Bottom]
      blockClass (HS.fromList ["is-clipped"]) $ buttonEleClass (HS.fromList ["is-large"]) (text "bad") >>= modal (image (HS.fromList []) "https://www.gardeningknowhow.com/wp-content/uploads/2020/04/Tomatoes.jpg") never
      block $
        buttonEleClass (HS.fromList ["is-large"]) (text "card")
          >>= modalCard
            "This is a card"
            (Just $ never <$ image (HS.fromList ["is-128x128"]) "https://www.gardeningknowhow.com/wp-content/uploads/2020/04/Tomatoes.jpg")
            Nothing
            never
      block $
        cardClass
          mempty
          (Just $ cardHeader "Card" (Just $ const (never <$ icon "fas fa-retweet")))
          (Just $ never <$ image (HS.fromList ["is-128x128"]) "https://www.gardeningknowhow.com/wp-content/uploads/2020/04/Tomatoes.jpg")
          Nothing
          Nothing
    columnClass (fromList []) $ do
      void $ switchDyn <$> widgetHold (pure never) (never <$ notificationClass (fromList ["is-primary"]) "fhoi wefweoj fweof jweofjweo" <$ ready)
      void $ switchDyn <$> widgetHold (pure never) (never <$ notificationClass (fromList ["is-danger"]) "fjowe jfow ejfo wefj wfwl fjli ejfw fjwe fnwi fnwi nfwf fwoj foei wfie rfir efiu ergi uerh giue rhfj erjf oiqw ejfo wejf owei jfwo efjw oefj owef jwoe fjoe wjf\nu \n ibf feiw hfie whfi wefj iwfj iwef iewf jweo fjwo efjo wjfo wejf owej fjno iwef weoj fweo fjwe ofjweo" <$ ready)
  dClock <- liftIO getCurrentTime >>= clockLossy 1
  let showTick = (15 * 60 -) . fromIntegral . _tickInfo_n
  dangerMessage "Clock" $ display $ showTick <$> dClock
  -- divClass "container" $
  columnsClass (fromList ["is-multiline", "is-centered"]) $ do
    -- replicateM_ 3 $ divClass "column is-4" $ dangerCard "Clock" $ dynText $ showTick <$> clock
    columnClass (singleton "is-12") timer >>= accumDyn (\_ i -> show i) "" >>= columnClass (fromList ["is-12", "is-align-content-center"]) . dynText
    let helper n = replicateM_ n $ columnClass (singleton $ "is-" <> show (12 `div` n)) timer
    mapM_ helper [2, 3, 4, 6, 12]
    -- divClass "column is-two-fifths" $ dangerCard "Clock" $ dynText $ showTick <$> dClock
    -- divClass "column" $ dangerCard "Clock" $ dynText $ showTick <$> dClock
    -- elAttr "div" ("class" =: "column" <> "style" =: "width: 38.1971%; flex: none") $ dangerCard "Clock" $ dynText $ showTick <$> dClock
    divClass "column" blank
