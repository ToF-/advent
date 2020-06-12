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
writeIntcode pgm 0 code = code : tail pgm
writeIntcode pgm n code = (head pgm) : writeIntcode (tail pgm) (pred n) code
