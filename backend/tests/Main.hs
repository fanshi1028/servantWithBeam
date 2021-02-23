module Main where

import Hedgehog (classify, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@=?), (@?=))
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)
import Universum

simpleMathTests :: TestTree
simpleMathTests = testCase "simple math" $ (1 :: Integer) + 2 @?= 3

simpleMathTests1 :: TestTree
simpleMathTests1 = testCase "1 + 3" $ (1 :: Integer) + 3 @?= 4

simplePropTest :: Int -> TestTree
simplePropTest start = testProperty (show start) $
  property $ do
    xs <- forAll $ Gen.list (Range.linear start 10) Gen.alpha
    classify "empty" $ null xs
    classify "small" $ length xs <= 5
    classify "large" $ length xs > 7
    length (drop 1 xs) === length xs - 1

simpleGoldenTest :: TestTree
simpleGoldenTest = goldenVsString "golden" "./tests/testResult.json" $ do
  return "Hi"

moreMathTests :: TestTree
moreMathTests =
  testGroup
    "more math"
    [ simpleMathTests,
      simpleMathTests1,
      testCase "on9" $ (6 :: Integer) @=? 1 + 1 + 4,
      simplePropTest 1,
      expectFail $ simplePropTest 0,
      -- simplePropTest 0,
      simpleGoldenTest
    ]

main :: IO ()
main = do
  defaultMainWithRerun moreMathTests
