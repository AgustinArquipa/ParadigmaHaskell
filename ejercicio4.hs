--Listas por Comprension
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{- Sumar Cuadrados 
sumarCuadrados :: Integer -> Integer 
suma de los cuadrados de los primeros n numeros, es decir
1^2 + 2² + 3² + ...
-}

sumarCuadrados :: Integer -> Integer 
sumarCuadrados m 
 | m == 0 = 0
 | otherwise  = m^2 + sumarCuadrados (m-1)

{- replica :: Int -> a -> [a]
Es la lista formada por n copias del elemento. Por ejemplo
replica 3 True == [True, True, True]
-}
replica :: Int -> a -> [a]
replica a elem
 | a == 0 = []
 | otherwise = elem : replica (a-1) elem