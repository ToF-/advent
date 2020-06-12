module D01ASpec where
import Test.Hspec
import D01A

spec :: SpecWith ()
spec = do
    describe "dummy" $ do
        it "should demonstrate test harness" $ do
            let n = 2 :: Integer
            n + n `shouldBe` 4

    describe "required fuel" $ do
        it "for a mass of 12 should be 2" $ do
            requiredFuel 12 `shouldBe` 2
