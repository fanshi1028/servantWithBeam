{-# LANGUAGE RecursiveDo #-}

module Utils.Bulma.Components.Modal where

import Control.Monad.Fix (MonadFix)
import Data.HashSet (fromList)
import qualified Data.HashSet as HS
import Reflex.Dom
import Universum
import Utils.Bulma.Elements (bulma, bulma', bulmaDyn, delete)
import Utils.Common (Classes, maybeWidget)

-- FIXME is-clipped to <html> to stop background scrolling

modalInternal :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => m a -> Event t () -> Event t b -> m a
modalInternal mContent closeE openEv = do
  rec clss <-
        accumDyn
          ( \_ -> \case
              Left _ -> mempty
              Right _ -> HS.fromList ["is-active"]
          )
          mempty
          $ leftmost [Right <$> openEv, Left <$> leftmost [closeE, closeE']]
      (closeE', contentE) <- bulmaDyn "div" "modal" clss $ do
        backgroundCloseE <- domEvent Click . fst <$> bulma' "div" "modal-background" mempty blank
        contentE' <- mContent
        buttonCloseE <- domEvent Click . fst <$> bulma' "button" "modal-close" (fromList ["is-large"]) blank
        return (leftmost [backgroundCloseE, buttonCloseE], contentE')
  return contentE

modalCardClassInternal ::
  (DomBuilder t m, MonadHold t m, MonadFix m) =>
  -- | class of the outter div
  Classes ->
  -- | title
  Text ->
  -- | body
  Maybe (m (Event t a)) ->
  -- | footer
  Maybe (m (Event t b)) ->
  m (Event t (), Event t (Either a b))
modalCardClassInternal clss title mBody mFoot = bulma "div" "modal-card" clss $ do
  delE <- elClass "header" "modal-card-head" $ do
    elClass "p" "modal-card-title" $ text title
    delete blank
  bodyE <- Left <<$>> maybeWidget (elClass "section" "modal-card-body") mBody
  footE <- Right <<$>> maybeWidget (elClass "footer" "modal-card-foot") mFoot
  return (delE, bodyE <> footE)

modal :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => m a -> Event t () -> Event t b -> m a
modal = modalInternal . divClass "modal-content"

modalCard :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Text -> Maybe (m (Event t a)) -> Maybe (m (Event t b)) -> Event t () -> Event t c -> m (Event t (Either a b))
modalCard title mBody mFoot closeE openE = do
  rec (closeE', otherEvs) <- modalInternal (modalCardClassInternal mempty title mBody mFoot) (leftmost [closeE, closeE']) openE
  return otherEvs
