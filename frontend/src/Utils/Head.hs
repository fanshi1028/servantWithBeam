{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

module Utils.Head (headElement, myMainWidget) where

import Reflex.Dom (MonadWidget, Widget, blank, el, elAttr, mainWidgetWithHead, text, (=:))
import Universum

headElement :: MonadWidget t m => m ()
headElement = do
  el "title" $ text "Currency Rates Fetcher"
  meta $ ("name" =: "viewport") <> ("content" =: "width=device-width, initial-scale=1")
  meta $ "charset" =: "utf-8"
  elAttr "script" ("src" =: "https://kit.fontawesome.com/182812e72f.js" <> "crossorigin" =: "anonymous") blank
  styleSheet "https://cdn.jsdelivr.net/npm/bulma@0.9.1/css/bulma.min.css"
  styleSheet "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"
  where
    -- styleSheet "/frontend/css/simple.css"
    styleSheet l = elAttr "link" (("rel" =: "stylesheet") <> ("href" =: l)) blank
    meta attr = elAttr "meta" attr blank

-- styleSheet l = elAttr "link" (("rel" =: "stylesheet") <> ("type" =: "text/css") <> ("href" =: l)) blank
-- styleSheet l = elAttr "link" (("rel" =: "stylesheet") <> ("type" =: "text/html") <> ("href" =: l)) blank
-- styleSheet l = elAttr "link" (("type" =: "text/css") <> ("href" =: l)) blank

myMainWidget :: (forall x. Widget x ()) -> IO ()
myMainWidget = mainWidgetWithHead headElement
