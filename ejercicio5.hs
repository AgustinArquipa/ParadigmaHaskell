{-
Definir una funcion que multiplique 
un escalar x una matriz
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char

-- Una funcion auxiliar que multiplica cada elemento de la lista por escalar
multList :: Float -> [Float] -> [Float]
multList a = map (a *)

multMatriz :: Float -> [[Float]] -> [[Float]]
multMatriz 0 _ = error "escalar es nulo!"
multMatriz _ [] = [] -- Caso base: Multiplicar un escalar por una lista vacia
multMatriz a (x:xs) = multList a x : multMatriz a xs

{- El guion bajo en Haskell se utiliza como un comodin cuando no estamos interesados en un valor especifico.
En otras palabras se usa para indicar que no necesitas el valor de una variable en una expresion o patron, pero aun asi
debes mencionarla para que haskell pueda analizar y hacer coincidir los patrones correctamente
-}

sumarList :: [Float] -> [Float] -> [Float]
sumarList [] [] = []
sumarList ys [] = ys --Otro caso base, en caso de que una lista no este vacia
sumarList [] ys = ys --Otro caso base
sumarList (x:xs) (c:cs) = (x + c) : sumarList xs cs

--Funcion para sumar matrices
sumarMatrices :: [[Float]] -> [[Float]] -> [[Float]]
sumarMatrices [] [] = []
sumarMatrices (x:xs) (c:cs) = sumarList x c : sumarMatrices xs cs

--Ejercicio de cadena

cadena2NumAux :: String -> String --Metodo auxiliar que me devuelve la cadena
cadena2NumAux [] = []
cadena2NumAux (x:xs)
 | isDigit x = x : cadena2NumAux xs
 | otherwise = cadena2NumAux xs

cadena2Num :: String -> Int --Ahora simplemente reutilizo el codigo anterior y lo paso a numero
cadena2Num str = read (cadena2NumAux str)
--El metodo read tranforma una cadena si es que contiene numero, a un string

--resto :: Integer -> Integer -> Integer
-- resto por resta recursiva
resto :: Integer -> Integer -> Integer
resto a b
 | b == 0 = error "Divideno igual a cero."
 | a < b = a
 | otherwise = resto (a-b) b

{-
Cociente :: Integer -> Integer -> Integer
cociente por resta recursiva
-}
cociente :: Integer -> Integer -> Integer
cociente a b
 | b == 0 = error "Dividendo igual a cero."
 | a < b = 0
 | otherwise = 1 + cociente (a-b) b

{-
sumatoria :: Integer -> Integer -> Integer
retorna la sumatoria de un a -> b, chequear el intervalo
-}
sumatoria :: Integer -> Integer -> Integer
sumatoria a b
 | a > b = 0
 | otherwise = a + sumatoria (a+1) b