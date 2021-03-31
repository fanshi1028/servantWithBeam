{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Widgets.Currency (currencyFetcher) where

import Chronos (Date, builder_Ymd)
import qualified Data.Map as Map (fromList)

import qualified Data.Set as Set
import Data.Text (split)
import Data.Text.Internal.Builder (toLazyText)
import Reflex.Dom hiding ((.~))
import Universum
import Utils.Components (primaryMessage, checkBoxOf, navbar)

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

currencyFetcher :: MonadWidget t m => m ()
currencyFetcher =
  divClass "block" $ do
    -- divClass "box" $ elClass "h1" "title is-primary" $ text "Currency Rates Fetcher"
    navbar "Currency Rates Fetcher" "money"
    -- (bcdd, targets) <- divClass "columns is-center is-2" $ do
    -- elClass "span" "icon" $ elClass "i" "fas fa-retweet" blank
    (bcdd, targets) <- divClass "box columns is-center is-multiline" $ do
      bcdd <-
        divClass "column is-5" $
          primaryMessage "Choose base currency: " $ dropdown USD (constDyn currencies) def
      -- & dropdownConfig_attributes .~ constDyn ("class" =: "dropdown")
      -- & dropdownConfig_attributes .~ constDyn ("multiple" =: "true" <> "size" =: "4")
      targets <-
        divClass "column is-7" $
          -- divClass "box" $ do
          primaryMessage "Choose target currencies(If no targets are checked, it return all): " $
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
        -- gotEvResult <- ("progress is-info is-hidden" <$) <$> headE $ ffilter (\case
        gotEvResult =
          headE $
            ffilter
              ( \case
                  Left _ -> False
                  Right r -> isJust $ _xhrResponse_responseText r
              )
              evRsp
    divClass "box columns is-center" $
      divClass "column" $ do
        gotEvResult >>= holdDyn "progress is-info" . ($> "is-hidden") >>= flip (elDynClass "progress") blank
        primaryMessage "Response:" . el "pre" . dynText =<< holdDyn "Waiting for initial api response" evResult
    return ()
