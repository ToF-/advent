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

                it "in two positions" $ do
                    let p = intcode [1,5,6,7,99,10,20,00]
                    run p `shouldBe` [1,5,6,7,99,10,20,30]

                it "in any position" $ do
                    let p = intcode [1,6,7,8,99,00,10,20,00]
                    run p `shouldBe` [1,6,7,8,99,00,10,20,30]

            it "performing a multiplication" $ do
                    let p = intcode [2,5,6,7,99,42,17,00]
                    run p `shouldBe` [2,5,6,7,99,42,17,714]

            describe "chaining operations" $ do
                it "in one order" $ do
                    let p = intcode [1,09,10,11
                                    ,2,09,10,12
                                    ,99
                                    ,42,17,00,00]
                    run p `shouldBe` [1,09,10,11
                                     ,2,09,10,12
                                     ,99
                                     ,42,17,59,714]
                it "in any order" $ do
                    let p = intcode [2,09,10,11
                                    ,1,09,10,12
                                    ,99
                                    ,42,17,00,00]
                    run p `shouldBe` [2,09,10,11
                                     ,1,09,10,12
                                     ,99
                                     ,42,17,714,59]
        describe "other examples" $ do
            it "rewriting initial position" $ do
                let p = intcode [ 1,9,10,3
                                , 2,3,11,0
                                , 99
                                , 30,40,50]
                run p `shouldBe` [3500,9,10,70
                                 , 2,3,11,0
                                 , 99
                                 , 30,40,50]

            it "addition in place" $ do
                run (intcode [1,0,0,0,99]) `shouldBe` [2,0,0,0,99]

            it "using opcodes" $ do
                run (intcode [2,4,4,5,99,0])  `shouldBe` [2,4,4,5,99,9801]

