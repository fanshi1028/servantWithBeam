{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Utils.Bulma.Elements where

import Control.Monad.Fix (MonadFix)
import Data.HashSet (insert, singleton)
import Reflex.Dom hiding (button, tag, (.~))
import Universum hiding (Element)
import Utils.Animate (dynAnimeClass)
import Utils.Common (Classes, mkClosable)

-- bulma helper

type Bulma = forall t m a. DomBuilder t m => m a -> m a

type BulmaWithClass =
  -- | extra classes configuration
  Classes ->
  Bulma

type BulmaWithDynClass =
  forall t.
  -- | extra classes configuration
  Dynamic t Classes ->
  Bulma

type Bulma' = forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)

type BulmaWithClass' =
  -- | extra classes configuration
  Classes ->
  Bulma'

bulma ::
  -- | HTML tag
  Text ->
  -- | main bulma class
  Text ->
  BulmaWithClass
-- bulma ele bulmaCls = elClass ele . appendClasses bulmaCls
bulma ele bulmaCls = elClass ele . unwords . toList . insert bulmaCls

bulmaDyn ::
  (PostBuild t m, DomBuilder t m) =>
  -- | HTML tag
  Text ->
  -- | main bulma class
  Text ->
  Dynamic t Classes ->
  m a ->
  m a
bulmaDyn ele bulmaCls = elDynClass ele . (unwords . toList . insert bulmaCls <$>)

bulma' ::
  DomBuilder t m =>
  -- | HTML tag
  Text ->
  -- | main bulma class
  Text ->
  -- | extra classes configuration
  Classes ->
  m a ->
  m (Element EventResult (DomBuilderSpace m) t, a)
bulma' ele bulmaCls = elClass' ele . unwords . toList . insert bulmaCls

bulmaDyn' ::
  (PostBuild t m, DomBuilder t m) =>
  -- | HTML tag
  Text ->
  -- | main bulma class
  Text ->
  Dynamic t Classes ->
  m a ->
  m (Element EventResult (DomBuilderSpace m) t, a)
bulmaDyn' ele bulmaCls = elDynClass' ele . (unwords . toList . insert bulmaCls <$>)

-- column

columnsClass :: BulmaWithClass
columnsClass = bulma "div" "columns"

columns :: Bulma
columns = columnsClass mempty

columnsClass' :: BulmaWithClass'
columnsClass' = bulma' "div" "columns"

columns' :: Bulma'
columns' = columnsClass' mempty

columnClass :: BulmaWithClass
columnClass = bulma "div" "column"

column :: Bulma
column = columnClass mempty

columnClass' :: BulmaWithClass'
columnClass' = bulma' "div" "column"

column' :: Bulma'
column' = columnClass' mempty

-- block

blockClass :: BulmaWithClass
blockClass = bulma "div" "block"

block :: Bulma
block = blockClass mempty

blockClass' :: BulmaWithClass'
blockClass' = bulma' "div" "block"

block' :: Bulma'
block' = blockClass' mempty

-- box

boxClass :: BulmaWithClass
boxClass = bulma "div" "box"

box :: Bulma
box = boxClass mempty

boxClass' :: BulmaWithClass'
boxClass' = bulma' "div" "box"

box' :: Bulma'
box' = boxClass' mempty

formBoxClass :: BulmaWithClass
formBoxClass = bulma "form" "box"

formBox :: Bulma
formBox = formBoxClass mempty

formBoxClass' :: BulmaWithClass'
formBoxClass' = bulma' "form" "box"

formBox' :: Bulma'
formBox' = formBoxClass' mempty

-- button

buttonClass :: (DomBuilder t m, Show a) => Classes -> a -> m (Event t a)
buttonClass clss event = (event <$) . domEvent Click . fst <$> bulma' "button" "button" clss (text $ show event)

button :: (DomBuilder t m, Show a) => a -> m (Event t a)
button = buttonClass mempty

buttonEleClass :: (DomBuilder t m) => Classes -> m a -> m (Event t ())
buttonEleClass clss ele = domEvent Click . fst <$> bulma' "button" "button" clss ele

buttonEle :: (DomBuilder t m) => m a -> m (Event t ())
buttonEle = buttonEleClass mempty

buttons' :: Classes -> (DomBuilder t m, Show a) => m [Event t a] -> m [Event t a]
buttons' = bulma "div" "buttons"

buttons :: (DomBuilder t m, Show a) => m [Event t a] -> m [Event t a]
buttons = buttons' mempty

-- content

contentClass :: BulmaWithClass
contentClass = bulma "div" "content"

content :: Bulma
content = contentClass mempty

contentClass' :: BulmaWithClass'
contentClass' = bulma' "div" "content"

content' :: Bulma'
content' = contentClass' mempty

-- delete

deleteClass :: DomBuilder t m => Classes -> m b -> m (Event t ())
deleteClass clss = domEvent Click . fst <<$>> bulma' "button" "delete" clss

delete :: DomBuilder t m => m b -> m (Event t ())
delete = deleteClass mempty

-- icon

iconClass :: (DomBuilder t m) => Classes -> Classes -> Text -> m ()
iconClass clss iconClss whatIcon = bulma "span" "icon" clss $ bulma "i" whatIcon iconClss blank

icon :: (DomBuilder t m) => Text -> m ()
icon = iconClass mempty mempty

iconTextClass :: Text -> BulmaWithClass
iconTextClass ele = bulma ele "icon-text"

iconTextSpanClass :: BulmaWithClass
iconTextSpanClass = iconTextClass "span"

iconTextSpan :: Bulma
iconTextSpan = iconTextSpanClass mempty

iconTextDivClass :: BulmaWithClass
iconTextDivClass = iconTextClass "div"

iconTextDiv :: Bulma
iconTextDiv = iconTextDivClass mempty

-- image

image :: DomBuilder t m => Classes -> Text -> m ()
image clss src = bulma "figure" "image" clss $ elAttr "img" ("src" =: src) blank

image' :: DomBuilder t m => Classes -> Text -> m (Element EventResult (DomBuilderSpace m) t, ())
image' clss src = bulma' "figure" "image" clss $ elAttr "img" ("src" =: src) blank

-- notification

notificationClass :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m, MonadIO (Performable m), TriggerEvent t m, PerformEvent t m) => Classes -> Text -> m ()
notificationClass clss msg = mkClosable $ mdo
  dClass <-
    unwords . toList . (insert "notification" clss <>)
      <<$>> dynAnimeClass (Just "bounceInDown") [(outSoonE, "flash"), (outE, "fadeOutUp")]
  (delE, outSoonE, outE, killE) <- elDynClass "div" dClass $ do
    clickE <- delete blank >>= headE
    let afterbuild n = getPostBuild >>= delay n
    text $ msg <> " "
    el "strong" $ text "fjewoj foewjf"
    text $ msg <> " "
    (clickE,,,) <$> afterbuild 4 <*> afterbuild 6 <*> afterbuild 6.5
  return $ leftmost [delE, killE]

notification :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m, MonadIO (Performable m), TriggerEvent t m, PerformEvent t m) => Text -> m ()
notification = notificationClass mempty

-- progress bar

progressBarInternal :: (DomBuilder t m, PostBuild t m) => Classes -> Integer -> Dynamic t (Maybe Integer) -> m ()
progressBarInternal clss maxValue currentValue =
  let pgAttr = "class" =: unwords (toList $ insert "progress" clss) <> "maxValue" =: show maxValue
      currentAttr =
        ( \case
            Nothing -> pgAttr
            Just v -> pgAttr <> "value" =: show v
        )
          <$> currentValue
   in elDynAttr "progress" currentAttr blank

progressBarClass :: (DomBuilder t m, PostBuild t m) => Classes -> Integer -> Dynamic t Integer -> m ()
progressBarClass clss maxValue = progressBarInternal clss maxValue . (Just <$>)

progressBar :: (DomBuilder t m, PostBuild t m) => Integer -> Dynamic t Integer -> m ()
progressBar = progressBarClass mempty

indeterminateProgressBarClass :: (DomBuilder t m, PostBuild t m) => Classes -> m ()
indeterminateProgressBarClass clss = progressBarInternal clss 100 $ constDyn Nothing

indeterminateProgressBar :: (DomBuilder t m, PostBuild t m) => m ()
indeterminateProgressBar = indeterminateProgressBarClass mempty

progressBarInternal' :: (DomBuilder t m, PostBuild t m) => Classes -> Integer -> Dynamic t (Maybe Integer) -> m (Element EventResult (DomBuilderSpace m) t, ())
progressBarInternal' clss maxValue currentValue =
  let pgAttr = "class" =: unwords (toList $ insert "progress" clss) <> "maxValue" =: show maxValue
      currentAttr =
        ( \case
            Nothing -> pgAttr
            Just v -> pgAttr <> "value" =: show v
        )
          <$> currentValue
   in elDynAttr' "progress" currentAttr blank

progressBarClass' :: (DomBuilder t m, PostBuild t m) => Classes -> Integer -> Dynamic t Integer -> m (Element EventResult (DomBuilderSpace m) t, ())
progressBarClass' clss maxValue = progressBarInternal' clss maxValue . (Just <$>)

progressBar' :: (DomBuilder t m, PostBuild t m) => Integer -> Dynamic t Integer -> m (Element EventResult (DomBuilderSpace m) t, ())
progressBar' = progressBarClass' mempty

indeterminateProgressBarClass' :: (DomBuilder t m, PostBuild t m) => Classes -> m (Element EventResult (DomBuilderSpace m) t, ())
indeterminateProgressBarClass' clss = progressBarInternal' clss 100 $ constDyn Nothing

indeterminateProgressBar' :: (DomBuilder t m, PostBuild t m) => m (Element EventResult (DomBuilderSpace m) t, ())
indeterminateProgressBar' = indeterminateProgressBarClass' mempty

-- table

tableRow :: (DomBuilder t m) => [m a] -> m [a]
tableRow = el "tr" . mapM (el "th")

tableRow' :: (DomBuilder t m) => [m a] -> m (Element EventResult (DomBuilderSpace m) t, [a])
tableRow' = el' "tr" . mapM (el "th")

tableHeadRow :: (DomBuilder t m) => m h -> [m d] -> m [d]
tableHeadRow th trs = el "tr" $ el "th" th >> mapM (el "td") trs

tableHeadRow' :: (DomBuilder t m) => m h -> [m d] -> m (Element EventResult (DomBuilderSpace m) t, [d])
tableHeadRow' th trs = el' "tr" $ el "th" th >> mapM (el "td") trs

tableClass :: DomBuilder t m => Classes -> m h -> m f -> m b -> m b
tableClass clss tHead tFoot tBody = bulma "table" "table" clss $ el "thead" tHead >> el "tfoot" tFoot >> el "tbody" tBody

tableClass' :: DomBuilder t m => Classes -> m h -> m f -> m b -> m (Element EventResult (DomBuilderSpace m) t, b)
tableClass' clss tHead tFoot tBody = bulma' "table" "table" clss $ el "thead" tHead >> el "tfoot" tFoot >> el "tbody" tBody

table :: DomBuilder t m => m h -> m f -> m b -> m b
table = tableClass mempty

table' :: DomBuilder t m => m h -> m f -> m b -> m (Element EventResult (DomBuilderSpace m) t, b)
table' = tableClass' mempty

tableContainerClass :: BulmaWithClass
tableContainerClass = bulma "div" "table-container"

tableContainer :: Bulma
tableContainer = tableContainerClass mempty

tableContainerClass' :: BulmaWithClass'
tableContainerClass' = bulma' "div" "table-container"

tableContainer' :: Bulma'
tableContainer' = tableContainerClass' mempty

-- tags

tagClass :: BulmaWithClass
tagClass = bulma "span" "tag"

tag :: Bulma
tag = tagClass mempty

tagClass' :: BulmaWithClass'
tagClass' = bulma' "span" "tag"

tag' :: Bulma'
tag' = tagClass' mempty

tagsClass :: BulmaWithClass
tagsClass = bulma "div" "tags"

tags :: Bulma
tags = tagsClass mempty

tagsClass' :: BulmaWithClass'
tagsClass' = bulma' "div" "tags"

tags' :: Bulma'
tags' = tagsClass' mempty

tagDelete' :: DomBuilder t m => Classes -> m (Event t ())
tagDelete' clss = domEvent Click . fst <$> bulma' "a" "tag is-delete" clss blank

tagDelete :: DomBuilder t m => m (Event t ())
tagDelete = tagDelete' mempty

mkDeletableTag :: (MonadFix m, DomBuilder t m, MonadHold t m) => m a -> m ()
mkDeletableTag mtag = mkClosable $ tagsClass (singleton "has-addons") $ mtag >> tagDelete >>= headE

-- title

pTitleClass :: BulmaWithClass
pTitleClass = bulma "p" "title"

pTitle :: Bulma
pTitle = pTitleClass mempty

pTitleClass' :: BulmaWithClass'
pTitleClass' = bulma' "p" "title"

pTitle' :: Bulma'
pTitle' = pTitleClass' mempty

mkTitleClass :: Bool -> Integer -> BulmaWithClass
mkTitleClass sub n = bulma ("h" <> show n) (bool "" "sub" sub <> "title is-" <> show n)

subtitleClass :: Integer -> BulmaWithClass
subtitleClass = mkTitleClass True

mkTitleClass' :: Bool -> Integer -> BulmaWithClass'
mkTitleClass' sub n = bulma' ("h" <> show n) (bool "" "sub" sub <> "title is-" <> show n)

subtitleClass' :: Integer -> BulmaWithClass'
subtitleClass' = mkTitleClass' True

subtitle :: Integer -> Bulma
subtitle n = subtitleClass n mempty

titleClass :: Integer -> BulmaWithClass
titleClass = mkTitleClass False

subtitle' :: Integer -> Bulma'
subtitle' n = subtitleClass' n mempty

titleClass' :: Integer -> BulmaWithClass'
titleClass' = mkTitleClass' False

title :: Integer -> Bulma
title n = titleClass n mempty

title' :: Integer -> Bulma'
title' n = titleClass' n mempty
