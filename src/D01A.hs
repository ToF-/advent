module D01A 
    where

requiredFuel :: Integer -> Integer
requiredFuel mass = (mass `div` 3) - 2

totalRequiredFuel :: [Integer] -> Integer
totalRequiredFuel = sum . map requiredFuel
