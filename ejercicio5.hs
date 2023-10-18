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
 | a > b = 0 --Si a es mayor que b, la suma es 0
 | otherwise = a + sumatoria (a+1) b

cantDigitos :: Integer -> Integer
cantDigitos a
 | a == 0 = 0
 | otherwise = 1 + cantDigitos (a `div` 10)


--La manera correcta para encontrar el maximo de una lista , sin usar la funcion mas
{- 
maximo :: [Integer] -> Integer
maximo [] = error "Error lista vacia!"
maximo [x] = x --Caso base: lista con un solo elemento, el maximo es ese elemento
maximo (x:xs) = max x (maximo xs) 
-}
maximoLista :: Ord a => [a] -> a
maximoLista [] = error "Lista vacia!"
maximoLista [x] = x --Caso base
maximoLista (x:xs) = maximo x (maximoLista xs) --Compara x con el maximo del resto de la lista
--Funcion auxiliar para comparar dos elementos y devolver el maximo
maximo a b
 | a >= b = a
 |otherwise = b

--Funcion que dada una lista de enteros lo convierta a un solo numero
digitos2Enteros :: [Integer] -> Integer
digitos2Enteros lis = read (listaEnterosString lis)

listaEnterosString :: [Integer] -> String --funcion auxiliar
listaEnterosString = concatMap show

--Funcion para saber si un elemento pertenece al conjunto
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False  --Si la lista esta vacia, el elemento no esta
pertenece e (x:xs)
 | e == x = True
 | otherwise = pertenece e xs

--Funcion para sacar la interseccion de dos conjuntos
interseccion :: Eq a => [a] -> [a] -> [a]
interseccion [] _ = [] --Si la primer lista esta vacia
interseccion _ [] = [] --Si la segunda lista esta vacia
interseccion (x:xs) cs
 | x `elem` cs = x : interseccion xs cs --Si x esta en cs, lo incluye en la lista
 | otherwise = interseccion xs cs --sino lo sigue buscando

