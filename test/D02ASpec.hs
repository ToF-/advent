module D02ASpec where
import Test.Hspec
import D02A

spec :: SpecWith ()
spec = do
    describe "Intcode programs" $ do
        it "can be read at a position" $ do
            let p = intcode [1,9,10,3,2,3,11,0,99,30,40,50]
            p !! 9 `shouldBe` 30
            p !! 10 `shouldBe` 40

        it "can be written at a position" $ do
            let p = intcode [1,9,10,3,2,3,11,0,99,30,40,50]
            let p'=  (p ~! 11) 70

            p' `shouldBe`  intcode [1,9,10,3,2,3,11,0,99,30,40,70]

        it "can add values from two positions" $ do
            let p = intcode [1,9,10,3,2,3,11,0,99,30,40,50]
            addMem p 9 10 `shouldBe` 70

        it "can multiply values from two positions" $ do
            let p = intcode [1,9,10,3,2,3,11,0,99,30,40,50]
            mulMem p 9 10 `shouldBe` 1200

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

        describe "solves the puzzle input" $ do
            let p = intcode [ 1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,5,23,1,9,23,27
                            ,2,27,6,31,1,5,31,35,2,9,35,39,2,6,39,43,2,43,13,47,2,13,47,51
                            ,1,10,51,55,1,9,55,59,1,6,59,63,2,63,9,67,1,67,6,71,1,71,13,75
                            ,1,6,75,79,1,9,79,83,2,9,83,87,1,87,6,91,1,91,13,95,2,6,95,99,1
                            ,10,99,103,2,103,9,107,1,6,107,111,1,10,111,115,2,6,115,119,1,5
                            ,119,123,1,123,13,127,1,127,5,131,1,6,131,135,2,135,13,139,1
                            ,139,2,143,1,143,10,0,99,2,0,14,0]
            it "first replacing 1 an 2 with 12 and 2" $ do
                let q = writeIntcode (writeIntcode p 1 12) 2 2
                    r = run q
                readIntcode r 0 `shouldBe` 6568671


