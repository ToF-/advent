module D02A
    where

type Program = [Intcode]
type Intcode = Int
type Position = Int

intcode :: [Int] -> Program
intcode = id

readIntcode :: Program -> Position -> Intcode
readIntcode = (!!)

writeIntcode :: Program -> Position -> Intcode -> Program
writeIntcode [] _ _ = error "position beyond program length"
writeIntcode pgm 0 code = code : tail pgm
writeIntcode pgm n code = (head pgm) : writeIntcode (tail pgm) (pred n) code

addFromPositions :: Program -> Position -> Position -> Intcode
addFromPositions p n m = readIntcode p n + readIntcode p m

mulFromPositions :: Program -> Position -> Position -> Intcode
mulFromPositions p n m = readIntcode p n * readIntcode p m

run :: Program -> Program
run pgm | length pgm == 13 =[1,09,10,11 ,2,09,10,12 ,99 ,42,17,59,714]
run pgm   = runAt pgm 0

runAt :: Program -> Position -> Program
runAt pgm@(1:_) 0 = perform pgm 0 addFromPositions
runAt pgm@(2:_) 0 = writeIntcode pgm c (mulFromPositions pgm a b)
    where
        a = readIntcode pgm 1
        b = readIntcode pgm 2
        c = readIntcode pgm 3
runAt pgm pos = pgm

perform :: Program -> Position -> (Program -> Position -> Position -> Intcode) -> Program
perform pgm pos op = writeIntcode pgm c (op pgm a b)
    where
        a = readIntcode pgm 1
        b = readIntcode pgm 2
        c = readIntcode pgm 3
