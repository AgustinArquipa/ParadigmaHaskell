doble x = 2 * x

suma :: Num a => a -> a -> a
suma x y = x + y

añadirElementoLista elem lis = elem:lis
--El elemento : sirve para añadir un elemento a la lista

tamañoDeLista lis = length lis

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia lis = False

--Unir Dos Listas
unir2Lista :: [a] -> [a] -> [a]
unir2Lista l1 l2 = l1 ++ l2

--Aplanar lista
listaNoAplanada :: [[Integer]]
listaNoAplanada = [[2, 3], [4, 5, 6]]

listaAplanada :: [Integer]
listaAplanada = concat listaNoAplanada

tresPrimerasLestras :: [Char]
tresPrimerasLestras = take 3 "abcdegh"
--Esta funcion nos devuelve los tres primeros elementos de esa lista

--Ejemplo de combinacion de listas
nombres :: [[Char]]
nombres = ["Agustin", "Juan", "Sara"]
numerosTelefonos :: [Integer]
numerosTelefonos = [234, 387, 421]
listasCombinadas :: [([Char], Integer)]
listasCombinadas = zip nombres numerosTelefonos