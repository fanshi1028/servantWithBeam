module Utils.Components (checkBoxOf, primaryMessage, navbar,dangerMessage,mkCard,dangerCard) where

import Reflex.Dom hiding (checkbox, (.~))
import Universum

navbar :: (Monad m, DomBuilder t m) => Text -> Text -> m ()
navbar title icon = elClass "nav" "navbar is-primary" $ do
  divClass "navbar-brand" $ do blank
  divClass "navbar-item" $ do
    elClass "span" "icon-text" $ do
      elClass "span" "icon" $ elClass "i" ("fa fa-" <> icon) blank
      el "span" $ text title
  divClass "navbar-burger" blank

-- divClass "navbar-burger" $ do
--   let span = elAttr "span" ("aria-hidden" =: "true") blank
--   replicate 3 span

checkBox :: DomBuilder t m => m (InputElement EventResult (DomBuilderSpace m) t)
checkBox =
  inputElement $
    def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "checkbox" <> "class" =: "is-primary")
      & inputElementConfig_initialChecked .~ False

checkBoxOf :: (DomBuilder t m, Show a) => a -> m (a, InputElement EventResult (DomBuilderSpace m) t)
-- checkBoxOf c = divClass "is-control is-flex is-flex-wrap-wrap is-flex-direction-row" $
checkBoxOf c = elClass "label" "checkbox" $ do
  ele <- checkBox
  text $ show c
  return (c, ele)

mkMessage :: DomBuilder t m => Text -> Text -> m a -> m a
mkMessage primary header body = divClass ("message is-" <> primary) $ do
  divClass "message-header" $ text header
  divClass "message-body" body

primaryMessage :: DomBuilder t m => Text -> m a -> m a
primaryMessage = mkMessage "primary"

dangerMessage :: DomBuilder t m => Text -> m a -> m a
dangerMessage = mkMessage "danger"

mkCard :: DomBuilder t m => Text -> Text -> m a -> m ()
mkCard primary title content = divClass ("card is-" <> primary) $ do
  divClass "card-header" $ do
    elClass "span" "card-header-title" $ text title
    elClass "span" "card-header-icon" $ elClass "span" "icon" $ elClass "i" "fa fa-arrow" blank
  divClass "card-image" $ elAttr "img" ("src" =: "https://www.gardeningknowhow.com/wp-content/uploads/2020/04/Tomatoes.jpg") blank
  divClass "card-content" $ text "This is a clock, build by svg"
  divClass "card-footer" $ mapM (divClass "card-footer-item" . text) ["Start", "Stop"]
  return ()

dangerCard :: DomBuilder t m => Text -> m a -> m ()
dangerCard = mkCard "danger"
