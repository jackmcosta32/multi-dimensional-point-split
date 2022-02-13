module HelpersSpec where

import SpecHelper
import Helpers (str2Int, str2Float, splitString)

spec :: Spec
spec =
  describe "[Module] Helpers" $ do
    context "with function str2Int" $ do
      it "should return 1 for '1'" $
        str2Int "1" `shouldBe` 1

      it "should return 10 for '10'" $
        str2Int "10" `shouldBe` 10

      it "should return 100 for '100'" $
        str2Int "100" `shouldBe` 100
  
    context "with function str2Float" $ do
      it "should return 1.0 for '1.0'" $
        str2Float "1.0" `shouldBe` 1
    
      it "should return 1.1 for '1.1'" $
        str2Float "1.1" `shouldBe` 1.1
    
      it "should return 10 for '10'" $
        str2Float "10.0" `shouldBe` 10
    
      it "should return 10.1 for '10.1'" $
        str2Float "10.1" `shouldBe` 10.1

    context "with function splitString" $ do
      it "should split the given strign on the correct character" $
        splitString (==' ') "foo bar" `shouldBe` ["foo", "bar"]

      it "should split multiple matches" $ do
        splitString (==',') "1,2,3,4" `shouldBe` ["1", "2", "3", "4"]

main :: IO ()
main = hspec spec
    