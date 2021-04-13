module Utils.Bulma.Components.Breadcrumb where

import Data.HashMap.Internal (lookup)
import Data.HashSet (fromList)
import Reflex.Dom hiding (link)
import Universum
import Utils.Bulma.Elements (bulma)
import Utils.Common (Classes)

mkCrumb :: (DomBuilder t m, Show k) => Classes -> k -> Maybe Text -> m (Event t k)
mkCrumb clss crumb =
  elClass "li" (unwords $ toList clss)
    . ((crumb <$) . domEvent Click . fst <$>)
    . \case
      Nothing -> el' "a" $ text $ show crumb
      Just link -> elAttr' "a" ("href" =: link) (text $ show crumb)

breadcrumbClass :: (DomBuilder t m, Ord k, PostBuild t m, Show k, Hashable k) => Classes -> HashMap k Text -> [k] -> m (Event t k)
breadcrumbClass clss links bread = bulma "nav" "breadcrumb" clss $ el "ul" $ leftmost <$> sequence (mkCrumb'' bread)
  where
    mkCrumb' crumbClass crumb = mkCrumb crumbClass crumb $ lookup crumb links
    mkCrumb'' = \case
      [] -> []
      [crumb] -> [mkCrumb' (fromList ["is-active"]) crumb]
      crumb : acc -> mkCrumb' mempty crumb : mkCrumb'' acc

breadcrumb :: (DomBuilder t m, Ord k, PostBuild t m, Show k, Hashable k) => HashMap k Text -> [k] -> m (Event t k)
breadcrumb = breadcrumbClass mempty
