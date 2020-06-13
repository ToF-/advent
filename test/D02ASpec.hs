module D02ASpec where
import Test.Hspec
import D02A

spec :: SpecWith ()
spec = do
    describe "Intcode programs" $ do
        it "can be read at a position" $ do
            let p = intcode [1,9,10,3,2,3,11,0,99,30,40,50]
            readIntcode p 9 `shouldBe` 30
            readIntcode p 10 `shouldBe` 40

        it "can be written at a position" $ do
            let p = intcode [1,9,10,3,2,3,11,0,99,30,40,50]
            let p'= writeIntcode p 11 70

            p' `shouldBe`  intcode [1,9,10,3,2,3,11,0,99,30,40,70]

        it "can add values from two positions" $ do
            let p = intcode [1,9,10,3,2,3,11,0,99,30,40,50]
            addFromPositions p 9 10 `shouldBe` 70

        it "can multiply values from two positions" $ do
            let p = intcode [1,9,10,3,2,3,11,0,99,30,40,50]
            mulFromPositions p 9 10 `shouldBe` 1200

        describe "can be run" $ do
            it "halting at a 99 opcode" $ do
                let p = intcode [99,9,10,3,2,3,11,0,99,30,40,50]
                run p `shouldBe` [99,9,10,3,2,3,11,0,99,30,40,50]

            describe "performing an addition" $ do
                it "between any number" $ do
                    let p = intcode [1,5,6,7,99,42,17,00]
                    run p `shouldBe` [1,5,6,7,99,42,17,59]

                it "in two position" $ do
                    let p = intcode [1,5,6,7,99,10,20,00]
                    run p `shouldBe` [1,5,6,7,99,10,20,30]

                it "in any position" $ do
                    let p = intcode [1,6,7,8,99,00,10,20,00]
                    run p `shouldBe` [1,6,7,8,99,00,10,20,30]

