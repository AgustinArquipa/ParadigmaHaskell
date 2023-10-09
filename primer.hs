doble :: Float -> Float
doble x = x + x

suma :: Int -> Int -> Int
suma x y = x + y

positivo :: Int -> Bool
positivo x = x > 0 

pertenece :: Int -> Bool
pertenece x = x >= 0 && x <= 9

multiploDeTres :: Int -> Bool
multiploDeTres x = x `mod` 3 == 0

celsius2Fahr :: Float -> Float
celsius2Fahr x = (x * 9/5) + 32