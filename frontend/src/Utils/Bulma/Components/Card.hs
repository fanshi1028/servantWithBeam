{-# LANGUAGE RecursiveDo #-}

module Utils.Bulma.Components.Card where

import Control.Monad.Fix (MonadFix)
import Reflex.Dom
import Universum
import Utils.Bulma.Elements
import Utils.Common (Classes, maybeWidget)

-- card

-- card-footer-item

aCardFooterItemClass :: BulmaWithClass
aCardFooterItemClass = bulma "a" "card-footer-item"

aCardFooterItem :: Bulma
aCardFooterItem = aCardFooterItemClass mempty

aCardFooterItemClass' :: BulmaWithClass'
aCardFooterItemClass' = bulma' "a" "card-footer-item"

aCardFooterItem' :: Bulma'
aCardFooterItem' = aCardFooterItemClass' mempty

pCardFooterItemClass :: BulmaWithClass
pCardFooterItemClass = bulma "p" "card-footer-item"

pCardFooterItem :: Bulma
pCardFooterItem = pCardFooterItemClass mempty

pCardFooterItemClass' :: BulmaWithClass'
pCardFooterItemClass' = bulma' "p" "card-footer-item"

pCardFooterItem' :: Bulma'
pCardFooterItem' = pCardFooterItemClass' mempty

buttonCardFooterItemClass :: BulmaWithClass
buttonCardFooterItemClass = bulma "button" "card-footer-item"

buttonCardFooterItem :: Bulma
buttonCardFooterItem = buttonCardFooterItemClass mempty

buttonCardFooterItemClass' :: BulmaWithClass'
buttonCardFooterItemClass' = bulma' "button" "card-footer-item"

buttonCardFooterItem' :: Bulma'
buttonCardFooterItem' = buttonCardFooterItemClass' mempty

cardHeader :: (DomBuilder t m, MonadFix m) => Text -> Maybe (Event t a -> m (Event t a)) -> m (Event t b)
cardHeader cardTitle mIcon = do
  bulma "p" "card-header-title" mempty $ text cardTitle
  rec iconE <- maybeWidget (bulma "button" "card-header-icon" mempty) (($ iconE) <$> mIcon)
  pure never

-- cardFooter :: DomBuilder t m => [ m (Event t a) ] -> m [Event t a]
-- cardFooter footerItems = bulma "a" "card-footer-item" mempty <<$>> footerItems

cardClass ::
  (DomBuilder t m, MonadHold t m, MonadFix m) =>
  -- | class of the outter div
  Classes ->
  -- | header
  Maybe (m (Event t a)) ->
  -- | image
  Maybe (m (Event t b)) ->
  -- | body
  Maybe (m (Event t c)) ->
  -- | footer
  Maybe (m (Event t d)) ->
  m (Event t b, Event t c, Event t d)
cardClass clss mHeader mImage mBody mFoot = bulma "div" "card" clss $ do
  void $ maybeWidget (divClass "card-header") mHeader
  imageE <- maybeWidget (divClass "card-image") mImage
  contentE <- maybeWidget (elClass "section" "card-content") mBody
  footE <- maybeWidget (elClass "footer" "card-footer") mFoot
  return (imageE, contentE, footE)
