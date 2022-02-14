module DistancesSpec where

import SpecHelper
import Distances (euclideanDistance)

spec :: Spec
spec = 
  describe "[Module] Distances" $
    context "with function euclideanDistance" $ do
      it "should return 0 for two identical points" $
        euclideanDistance [1, 1] [1, 1] `shouldBe` 0

      it "should return 2 for [0, 0], [0, 2]" $
        euclideanDistance [0, 0] [0, 2] `shouldBe` 2
    
      it "should return 2 for [2, 4], [2, 2]" $
        euclideanDistance [2, 4] [2, 2] `shouldBe` 2

      it "should return 5 for [0, 3], [4, 0]" $
        euclideanDistance [0, 3] [4, 0] `shouldBe` 5

main :: IO ()
main = hspec spec
    