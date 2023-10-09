--ptoMedio :: (Float, Float) -> (Float, Float) -> (Float, Float)
--ptoMedio () = ()

listar :: Int -> Int -> Int -> [Int]
listar a b c = [a, b, c]

--Rango de pasos para una lista, es el for(i; i++; n)
rangoDePasos :: Int -> Int -> Int -> [Int]
rangoDePasos a b c = [a,(a+c)..b]

--Factorial de un numero
fac_p :: Int -> Int
fac_p 0 = 1
fac_p x = x * fac_p (x-1)

--Segunda manera de definir funciones usando GUARDAS => |
fac_G1 :: Int -> Int
fac_G1 x
    | x == 0 = 1
    | x >= 1 = x * fac_G1 (x-1)

--Variacion del Fac_G1 
fac_G2 :: Int -> Int
fac_G2 x 
    | x == 0 = 1 | otherwise = x * fac_G2 (x-1)

--Para obtener la cabeza de una lista
cabeza :: [Int] -> Int
cabeza (x:xs) = x

--Manera para iterar una lista
tam :: [a] -> Int
tam [] = 0
tam (x:xs) = 1 + tam xs