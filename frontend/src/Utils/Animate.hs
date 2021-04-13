module Utils.Animate where

import Control.Monad.Fix (MonadFix)
import Data.HashSet (insert, singleton)
import Reflex.Dom
import Universum
import Utils.Common (Classes)

type Animation = Text

dynAnimeClass :: (MonadFix m, Reflex t, MonadHold t m) => Maybe Text -> [(Event t a, Text)] -> m (Dynamic t Classes)
dynAnimeClass initAnime evWithAnime =
  insert "animate__animated" <<$>> do
    let mkAnimeEv = \(ev, anime) -> const (singleton $ "animate__" <> anime) <$ ev
        initClass = case initAnime of
          Nothing -> mempty
          Just anime -> singleton $ "animate__" <> anime
    foldDyn ($) initClass (mergeWith (.) $ mkAnimeEv <$> evWithAnime)
