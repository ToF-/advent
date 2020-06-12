module D02ASpec where
import Test.Hspec
import D02A

spec :: SpecWith ()
spec = do
    describe "Intcode programs" $ do
        it "can be read at a position" $ do
            let p = intcode [0,9,10,3,2,3,11,0,99,30,40,50]
            readIntcode p 9 `shouldBe` 30
            readIntcode p 10 `shouldBe` 40

        it "can be written at a position" $ do
            let p = intcode [0,9,10,3,2,3,11,0,99,30,40,50]
            let p'= writeIntcode p 11 70

            p' `shouldBe`  intcode [0,9,10,3,2,3,11,0,99,30,40,70]
