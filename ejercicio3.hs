{- EJERCICIOS SOBRE LISTAS 
Limpiar :: String -> String -> String 
elimina todas las apariciones de cualquier caracter de la primera cadena en la segunda 
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.Random
import Data.Hashable


--Para elegir un caracter random vamos a elejir un indice random pasado de una cadena y con !! devolvemos esa posicion
posicionAleatoria :: String -> Int
posicionAleatoria cad =
    let hashValue = hash cad :: Int
        longitud = length cad
        indice = abs (hashValue `mod` longitud)
    in indice

--Metodo para recuperar el caracter segun la posicion
caracterRandom :: String -> Char
caracterRandom cad = cad !! posicionAleatoria cad

eliminarChar :: Char -> String -> String
eliminarChar char (x:xs)
 | char == x = eliminarChar char xs --Si el caracter coincide, omitelo y sigue con el resto de la cadena
 | otherwise = x : eliminarChar char xs -- Si no coincide, incluyelo en el resultado y sigue con el resto de la cadena

{-metodo para quitar elementos de una cadena
quitarElemento :: Char -> String -> String
quitarElemento e [] = []
quitarElemento e (x:xs)
 | (x == e) = quitarElemento e xs
 | otherwise = x : (quitarElemento e xs) -}

--Para eliminar las apariciones 
limpiar :: String -> String -> String
limpiar [] [] = []
limpiar cad1 (c:xs)
 | c == caracterRandom cad1 = limpiar cad1 xs
 | otherwise = c : limpiar cad1 xs

 {- diff :: [Float] -> [Float] 
 Devuelve la diferencia de cada uno con el promedio general
 -}
-- Las funciones sum y fromIntegral son metodos propios
promedioGeneral :: [Float] -> Float
promedioGeneral lis = sum lis / fromIntegral (length lis)

--Metodo que nos ayuda a calcular
diffHelper :: [Float] -> Float -> [Float]
diffHelper [] _ = [] --caso base
diffHelper (x:xs) promedio = (x - promedio) : diffHelper xs promedio

diff :: [Float] -> [Float]
diff lista = diffHelper lista (promedioGeneral lista)

{-
todosIguales :: [int] -> Bool
Indica si una lista de enteros tiene todos sus elementos iguales.
-}

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales (x:xs) = all (== x) xs
{-Si la lista tiene al menos un elemento (x), 
se utiliza la función all para verificar si todos los elementos restantes (xs) son iguales a x. 
Si todos los elementos son iguales, devuelve True, de lo contrario, devuelve False.-}

{-
repLista :: [Int] -> Int -> [Int]
repetir cada elemento de la lista n veces
-}

repLista :: [Int] -> Int -> [Int]
repLista lista n = concatMap (replicate n) lista

{- Lista2Lista
Dada una lista de enteros, devuelve una unica lista
con los elementos de todas las listas
-}
lista2Lista :: [[Int]] -> [Int]
lista2Lista = concat

{-
checkParentesis :: String -> Bool
Devuelve verdadero si la cantidad de parentesis '(' es la misma 
cantidad de parentesis ')'
-}
contadorAbierto :: [Char] -> Int
contadorAbierto [] = 0
contadorAbierto (x:xs)
 | x == '(' = 1 + contadorAbierto xs
 | otherwise = contadorAbierto xs

contadorCerrado :: Num p => [Char] -> p
contadorCerrado [] = 0
contadorCerrado (x:xs)
 | x == ')' = 1 + contadorCerrado xs
 | otherwise = contadorCerrado xs

checkParentesis cad = contadorAbierto cad == contadorCerrado cad

{-
finales :: Integer -> [Integer] -> [Integer]
Devuelve los n elementos finales de la lista
-}

finales :: Int -> [Integer] -> [Integer]
finales n lista
 | n <= 0 = []
 | length lista <= n = lista --Si la longitud de la lista es menor o igual a n devolvemos la lista
 | otherwise = drop (length lista - n) lista -- con drop eliminamos los primeros m elementos, donde m es el calculo longitud - n

iniciales :: Int -> [Integer] -> [Integer]
iniciales n lista
 | n <= 0 = [] --Caso base
 | length lista < n = lista
 | otherwise = take n lista -- Con el metodo take recupero los n elementos iniciales de la lista

{-
extremos :: Int -> [Integer] -> [Integer]
Devuelve los primeros n elementos de la lista junto con los n elementos finales
-}

extremos :: Int -> [Integer] -> [Integer]
extremos n lista
 | n <= 0 = [] --Caso base 1
 | length lista < n = lista
 | otherwise = iniciales n lista ++ finales n lista --Con el operador ++ unimos 2 listas

--Ejercicio practico Metodo Bisercion
--Funciones como parametros
f :: Double -> Double
f x = (x ^ 2) - 4 --Funcion cuadratica

f2 :: Double -> Double --Funcion exponencial
f2 x = exp x - log (x + 4)

biseccion :: (Double -> Double) -> (Double, Double) -> Double -> Double
biseccion f2 (a, b) epsilon
 | fa * fb > 0 = error "No cumple condicion"
 | abs fc <= epsilon =  c
 | fa * fc < 0 = biseccion f2 (a, c) epsilon
 | otherwise = biseccion f2 (c, b) epsilon
 where
    fa = f2 a
    fb = f2 b
    fc = f2 c
    c = (a + b) / 2

--Metodo de biseccion 2 que devuelve una tupla
biseccion2 :: (Double -> Double) -> (Double, Double) -> Double -> (Double, Int)
biseccion2 f2 (a, b) epsilon = biseccion2' f (a, b) epsilon 0
 where
    biseccion2' f (a, b) epsilon iter
     | fa * fb > 0 = error "No cumple condicion"
     | abs fc <= epsilon =  (c, iter)
     | fa * fc < 0 = biseccion2' f2 (a, c) epsilon (iter + 1)
     | otherwise = biseccion2' f2 (c, b) epsilon (iter + 1)
     where
        fa = f2 a
        fb = f2 b
        fc = f2 c
        c = (a + b) / 2

aplica :: (a -> b) -> [a] -> [b]
--Funcion de a hacia b, donde [a] dominio y [b] codominio es el famoso MAP
aplica _ [] = []
aplica f (x:xs) = f x : aplica f xs

filtra :: (a -> Bool) -> [a] -> [b] --Filtra equivale al filter
filtra - [] = []
filtra fb (x:xs)
 | f x = x : filtra xs
 | otherwise = filtra xs