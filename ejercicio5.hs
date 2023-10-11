{-
Definir una funcion que multiplique 
un escalar x una matriz
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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