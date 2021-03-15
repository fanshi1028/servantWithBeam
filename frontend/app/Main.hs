{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

import Chronos (Date (Date), builder_Ymd)
import Control.Monad.Fix (MonadFix)
import Data.Generics.Labels ()
import qualified Data.Map as Map (fromList)
import qualified Data.Set as Set
import Data.Text (split)
import Data.Text.Internal.Builder (toLazyText)
import Reflex.Dom hiding (checkbox, (.~))
import Text.Show (Show (showsPrec))
import Universum

data Period = Latest | StartFrom Date | History Date Date

data ApiCallConfig = ApiCallConfig
  { _base :: Maybe CurrencyCode,
    _targets :: Set CurrencyCode,
    _period :: Period
  }

data CurrencyCode = CAD | HKD | ISK | PHP | DKK | HUF | CZK | GBP | RON | SEK | IDR | INR | BRL | RUB | HRK | JPY | THB | CHF | EUR | MYR | BGN | TRY | CNY | NOK | NZD | ZAR | USD | MXN | SGD | AUD | ILS | KRW | PLN deriving (Show, Ord, Eq, Enum, Bounded)

currencies :: Map CurrencyCode Text
currencies = Map.fromList $ (id &&& show) <$> [minBound .. maxBound]

type Queries = [(Text, Text)]

buildQueries :: Queries -> Text
buildQueries = foldl' (\acc (q, v) -> acc <> (if acc == "" then "?" else "&") <> q <> "=" <> v) ""

buildReq :: ApiCallConfig -> XhrRequest ()
buildReq (ApiCallConfig mBase targets period) =
  let (path, qs) = case period of
        Latest -> ("/latest", qTB)
        StartFrom d1 -> ("/" <> formatD d1, qTB)
        History d1 d2 -> ("/history", qTB <> [("start_at", formatD d1), ("end_at", formatD d2)])
        where
          formatD = toStrict . toLazyText . builder_Ymd (Just '-')
          qB = maybe [] ((: []) . ("base",) . show) mBase
          qT =
            bool
              [ ( "symbols",
                  foldl'
                    ( \case
                        "" -> show
                        acc -> ((acc <> ",") <>) . show
                    )
                    ""
                    targets
                )
              ]
              []
              (null targets)
          qTB = qT <> qB
   in XhrRequest
        "GET"
        ("https://api.exchangeratesapi.io" <> path <> buildQueries qs)
        def

apiCallConfig :: (Period, (Maybe CurrencyCode, Set CurrencyCode)) -> ApiCallConfig
apiCallConfig (p, (b, t)) = ApiCallConfig b t p

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

component :: DomBuilder t m => Text -> m a -> m a
component header body = divClass "message is-primary" $ do
  divClass "message-header" $ text header
  divClass "message-body" body

-- component :: DomBuilder t m => Text -> m a -> m a
-- component header body = divClass "box" $ do
--         elClass "label" "label" $ text header
--         body

main1 :: MonadWidget t m => m ()
main1 =
  divClass "block" $ do
    -- divClass "box" $ elClass "h1" "title is-primary" $ text "Currency Rates Fetcher"

    elClass "nav" "navbar is-light  is-large" $ do
      divClass "navbar-brand" $ do blank
      divClass "navbar-item" $ do
        elClass "span" "icon-text is-larger" $ do
          elClass "span" "icon is-large" $ elClass "i" "fa fa-money" blank
          el "span" $ text "Currency Rates Fetcher"
      divClass "navbar-burger" blank
    -- divClass "navbar-burger" $ do
    --   let span = elAttr "span" ("aria-hidden" =: "true") blank
    --   replicate 3 span
    -- (bcdd, targets) <- divClass "columns is-center is-2" $ do
    -- elClass "span" "icon" $ elClass "i" "fas fa-retweet" blank
    (bcdd, targets) <- divClass "box columns is-center is-multiline" $ do
      bcdd <-
        divClass "column is-5" $
          component "Choose base currency: " $ dropdown USD (constDyn currencies) def
      -- & dropdownConfig_attributes .~ constDyn ("class" =: "dropdown")
      -- & dropdownConfig_attributes .~ constDyn ("multiple" =: "true" <> "size" =: "4")
      targets <-
        divClass "column is-7" $
          -- divClass "box" $ do
          component "Choose target currencies(If no targets are checked, it return all): " $
            divClass "tags is-multiline" $ sequence $ divClass "tag" . checkBoxOf <$> [minBound .. maxBound :: CurrencyCode]
      return (bcdd, targets)
    evStart <- getPostBuild
    let evTargetsChanged = traceEvent "ev2" $ leftmost $ (\(c, ev) -> (c,) <$> _inputElement_checkedChange ev) <$> targets
    dynTargets <- traceDyn "dyn" <$> foldDyn (\(code, chk) -> bool (Set.delete code) (Set.insert code) chk) mempty evTargetsChanged
    let dynQueries = zipDyn (constDyn Latest) (zipDyn (Just <$> value bcdd) dynTargets)
        evQueries =
          tagPromptlyDyn dynQueries $
            leftmost
              ( [ () <$ _dropdown_change bcdd,
                  () <$ evTargetsChanged,
                  evStart
                ]
              )
    evRsp <- performRequestAsyncWithError $ traceEvent "xhr" $ buildReq . apiCallConfig <$> evQueries
    let format = fromString @Text . intercalate "\n," . (toString <$>) . split (== ',')
        evResult = format . fromMaybe "Oops :Nothing in response" . either (Just . show) _xhrResponse_responseText <$> evRsp
    divClass "block columns is-center" $
      divClass "column" $
        component "Response:" . el "pre" . dynText =<< holdDyn "Waiting for initial api response" evResult
    return ()

headElement :: MonadWidget t m => m ()
headElement = do
  el "title" $ text "Currency Rates Fetcher"
  meta $ ("name" =: "viewport") <> ("content" =: "width=device-width, initial-scale=1")
  meta $ "charset" =: "utf-8"
  elAttr "script" ("src" =: "https://kit.fontawesome.com/182812e72f.js" <> "crossorigin" =: "anonymous") blank
  styleSheet "https://cdn.jsdelivr.net/npm/bulma@0.9.1/css/bulma.min.css"
  where
    -- styleSheet "/frontend/css/simple.css"
    styleSheet l = elAttr "link" (("rel" =: "stylesheet") <> ("href" =: l)) blank
    meta attr = elAttr "meta" attr blank

-- styleSheet l = elAttr "link" (("rel" =: "stylesheet") <> ("type" =: "text/css") <> ("href" =: l)) blank
-- styleSheet l = elAttr "link" (("rel" =: "stylesheet") <> ("type" =: "text/html") <> ("href" =: l)) blank
-- styleSheet l = elAttr "link" (("type" =: "text/css") <> ("href" =: l)) blank

main :: IO ()
main =
  -- mainWidgetWithCss $(embedFile "frontend/css/simple.css") main1
  mainWidgetWithHead headElement main1

-- mainWidget main1

-- divClass "box control" $ do
--   inputElement $
--     def & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("placeholder" =: "hiejfowejf")
-- elClass "span" "icon" $ elClass "i" "fas fa-retweet" blank
-- el "br" blank
-- text "Amount in base currency: "
-- b <- button "get exchange rate"
-- b1 <- dyn (button <$> value input)
-- kp <- keypress Enter _
