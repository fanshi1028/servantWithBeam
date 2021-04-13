{-# LANGUAGE RecursiveDo #-}
module Utils.Bulma.Components.Dropdown where

import Reflex.Dom hiding (button, dropdown)
import Universum
import Utils.Bulma.Elements (bulma, bulmaDyn, bulmaDyn', button, buttonEle)
import qualified Data.HashSet as HS

-- ddItem ele = elDynAttr' ele ("class" =: "")

ddDivider :: DomBuilder t m => m (Event t a)
ddDivider = elClass "hr" "dropdown-divider" $ pure never

-- dropdown initClss initTitle icon itemList closeEv openEv = do
--   rec
--   let dClss =
--         accumDyn
--           ( \acc -> \case
--               Left _ -> initClss
--               Right _ ->  HS.member "is-active" acc & bool (HS.insert "is-active") (HS.delete "is-active")
--           )
--           initClss
--           $ leftmost [ Left <$> closeEv, Right <$> openEv ]
--       dTitle = holdDyn initTitle ev
--   bulmaDyn "div" "dropdown" dClss $ do
--     toggleEv <- divClass "dropdown-trigger" $
--       buttonEle $ do
--         el "span" $ dynText dTitle
--         icon
--     divClass "dropdown-menu" $ divClass "dropdown-content" $ blank
