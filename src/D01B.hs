module D01B
    where


requiredFuel :: Integer -> Integer
requiredFuel 0 = 0
requiredFuel mass = fuel + (requiredFuel fuel)
    where fuel = max 0 ((mass `div` 3) - 2)

totalRequiredFuel :: [Integer] -> Integer
totalRequiredFuel = sum . map requiredFuel
