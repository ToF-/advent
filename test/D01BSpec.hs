module D01BSpec where
import Test.Hspec
import D01B

spec :: SpecWith ()
spec = do
    describe "required fuel" $ do
        it "for a mass of 12 should be 2" $ do
            requiredFuel 12 `shouldBe` 2
