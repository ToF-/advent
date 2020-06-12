module D01ASpec where
import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "dummy" $ do
        it "should demonstrate test harness" $ do
            let n = 2 :: Integer
            n + n `shouldBe` 4
