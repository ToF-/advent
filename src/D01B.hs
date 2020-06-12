module D01B
    where


requiredFuel :: Integer -> Integer
requiredFuel mass = (mass `div` 3) - 2

totalRequiredFuel :: [Integer] -> Integer
totalRequiredFuel = sum . map requiredFuel
