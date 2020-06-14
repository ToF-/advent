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

infix 6 ~!
(~!) :: Program -> Position -> (Intcode -> Program)
pgm ~! p = writeIntcode pgm p

add_ :: Program -> Position -> Position -> Intcode
add_ p n m = p !! n + p !! m

mulFromPositions :: Program -> Position -> Position -> Intcode
mulFromPositions p n m = p !! n * p !! m

run :: Program -> Program
run pgm   = runAt pgm 0

runAt :: Program -> Position -> Program
runAt pgm p = case readIntcode pgm p of
                99 -> pgm
                1  -> runAt pgm' p'
                    where
                        pgm' = perform pgm p add_
                        p'   = p+4
                2  -> runAt pgm' p'
                    where
                        pgm' = perform pgm p mulFromPositions
                        p'   = p+4

perform :: Program -> Position -> (Program -> Position -> Position -> Intcode) -> Program
perform pgm pos op = writeIntcode pgm c (op pgm a b)
    where
        a = readIntcode pgm (succ pos)
        b = readIntcode pgm (succ (succ pos))
        c = readIntcode pgm (succ (succ (succ pos)))
