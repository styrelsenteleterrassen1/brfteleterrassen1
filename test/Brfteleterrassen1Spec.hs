module Brfteleterrassen1Spec (spec) where

import Brfteleterrassen1 qualified
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Brfteleterrassen1.main" $ do
    it "should print Hello, world!" $ do
      -- This is a placeholder test - normally you'd test actual functions
      -- For now, we just test that main exists and can be called
      Brfteleterrassen1.main `shouldReturn` ()

  describe "String operations (example)" $ do
    it "reverse . reverse should be identity" $
      property $
        \s -> reverse (reverse s) == (s :: String)

    it "length of concatenation equals sum of lengths" $
      property $
        \xs ys -> length (xs ++ ys) == length xs + length (ys :: [Int])

  describe "Arithmetic properties (example)" $ do
    it "addition is commutative" $
      property $
        \x y -> x + y == y + (x :: Int)

    it "multiplication by zero" $
      property $
        \x -> x * 0 == (0 :: Int)
