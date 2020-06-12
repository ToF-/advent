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
