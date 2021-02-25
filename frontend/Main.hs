module Main where

import Control.Monad.Fix (MonadFix)
import Reflex
import Reflex.Dom
import Universum

main = mainWidget $
  el "div" $ do
    nx <- numberInput
    d <- dropdown Times (constDyn ops) def
    ny <- numberInput
    let values = zipDynWith (,) nx ny
        result = zipDynWith (\o (x, y) -> runOp o <$> x <*> y) (_dropdown_value d) values
        resultText = fmap (pack . show) result
    text " = "
    dynText resultText

numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
numberInput = do
  n <-
    inputElement $
      def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
  return . fmap (readMaybe . unpack) $ _inputElement_value n

data Op = Plus | Minus | Times | Divide deriving (Eq, Ord)

ops :: Map Op Text
ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]

runOp :: Fractional a => Op -> a -> a -> a
runOp s = case s of
  Plus -> (+)
  Minus -> (-)
  Times -> (*)
  Divide -> (/)
