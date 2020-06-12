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
        it "For a mass of 1969, should be 654" $ do
            requiredFuel 1969 `shouldBe` 654
        it "For a mass of 100756 should be 33583" $ do
            requiredFuel 100756 `shouldBe` 33583
