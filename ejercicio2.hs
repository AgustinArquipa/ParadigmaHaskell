import Data.Char

--ptoMedio :: Devuelve el punto medio entre dos p1 y p2
ptoMedio :: (Float, Float) -> (Float, Float) -> (Float, Float)
ptoMedio (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

--norma euclideana :: la norma 
normaEuclideana :: (Float, Float) -> Float
normaEuclideana (x1, y1) = sqrt (x1^2 + y1^2)

--Segundos a tiempo:: Dada la cantidad de segundos retorna la cantidad de horas, minutos, segundos
segundos2Tiempo :: Integer -> (Integer, Integer, Integer)
segundos2Tiempo seg = (seg `div` 3600, seg `div` 60, seg `mod` 60)

--Definicion de funciones en Haskell
esVocal :: Char -> Bool
esVocal c = c `elem` "aeiouAEIOU"

esConsonante :: Char -> Bool
esConsonante c = c `elem` "bcdfghjkmnñpqrstvwxzBCDFGHJKMNÑOPQRSTVWZ" 

esVocalDos :: Char -> Bool
esVocalDos c
 | (b == 'a') = True
 | (b == 'e') = True
 | (b == 'i') = True
 | (b == 'o') = True
 | (b == 'u') = True
 | otherwise = False
    where b = toLower c

--Metodo que cuenta la cantidad de vocales
contador :: String -> Int
contador [] = 0
contador (x:xs)
 | esVocal x = 1 + contador xs
 | otherwise = contador xs

cantCons :: String -> Int
cantCons [] = 0
cantCons (x:xs)
 | esVocal x = cantCons xs
 | otherwise = 1 + cantCons xs

--Metodo que cuenta consonates y vocales y retorna en la tupla 
cantVocCons :: String -> (Int, Int)
cantVocCons x = (contador x, cantCons x)

{-codigo MURCIELAGO
         0123456789 , es decir (m,0) y asi 
codigo = "MURCIELAGO"
codM = toLower codigo-}
esMurcielago :: Char -> Char
esMurcielago x 
 | (x == 'm') || (x == 'M') = '0'
 | (x == 'u') || (x == 'U') = '1'
 | (x == 'r') || (x == 'R') = '2'
 | (x == 'c') || (x == 'C') = '3'
 | (x == 'i') || (x == 'I') = '4'
 | (x == 'e') || (x == 'E') = '5'
 | (x == 'l') || (x == 'L') = '6'
 | (x == 'a') || (x == 'A') = '7'
 | (x == 'g') || (x == 'G') = '8'
 | (x == 'o') || (x == 'O') = '9' 
 | otherwise = x

codificar :: String -> String
codificar [] = [] --Caso base por eso me pedia una exeption
codificar (x:xs) = esMurcielago x : codificar xs