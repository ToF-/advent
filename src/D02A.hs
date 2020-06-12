module D02A
    where

type Program = [Intcode]
type Intcode = Int
type Position = Int

intcode :: [Int] -> Program
intcode = id

readIntcode :: Program -> Position -> Intcode
readIntcode = (!!)
