module Main(main) where

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

--import Basic

main :: IO ()
main = hspec simpleMathSpec

-- TODO: Add more cases!
simpleMathSpec :: Spec
simpleMathSpec = describe "Tests of our simple math function" $ do
  context "when the numbers are small" $
    it "Should match the our expected value" $
      add 3 4 `shouldBe` 7

add :: Int -> Int -> Int
add a b = a + b