module D01BSpec where
import Test.Hspec
import D01B

spec :: SpecWith ()
spec = do
    describe "required fuel" $ do
        it "for a mass of 12 should be 2" $ do
            requiredFuel 12 `shouldBe` 2
        it "for a mass of 1969 should be 654 + 216 + 70 + 21 + 5 = 966" $ do
            requiredFuel 1969 `shouldBe` 654 + 216 + 70 + 21 + 5
