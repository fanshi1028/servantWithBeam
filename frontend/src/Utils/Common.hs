{-# LANGUAGE RecursiveDo #-}
module Utils.Common where

import Reflex.Dom
import Universum
import Control.Monad.Fix (MonadFix)

type Class = Text

type Classes = HashSet Class

maybeWidget :: (Monad m, Reflex t) => (m (Event t a) -> m (Event t a)) -> Maybe (m (Event t a)) -> m (Event t a)
maybeWidget container = \case
  Nothing -> pure never
  Just widget -> container widget

mkClosable :: (Adjustable t m, MonadHold t m, MonadFix m) => m (Event t b) -> m ()
mkClosable eleWithCloseEv = do
  rec closeEv <- switchDyn <$> widgetHold eleWithCloseEv (pure never <$ closeEv)
  return ()

mkClosableWithEvs :: (Adjustable t m, MonadHold t m, MonadFix m) => m (Event t a, Event t b) -> m (Event t b)
mkClosableWithEvs eleWithEvs = do
  let start = Workflow $ do
        (closeEv, otherEvs) <- eleWithEvs
        pure (otherEvs, end <$ closeEv)
      end = Workflow $ pure (never, end <$ never)
  switchDyn <$> workflow start
