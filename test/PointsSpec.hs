module PointsSpec where

import SpecHelper
import Points (str2Point)

spec :: Spec
spec = 
  describe "[Module] Points" $
    context "with function str2Point" $
      it "should return [1,2,3,4] for '1,2,3,4'" $
        str2Point "1,2,3,4" `shouldBe` [1,2,3,4]

main :: IO ()
main = hspec spec
    