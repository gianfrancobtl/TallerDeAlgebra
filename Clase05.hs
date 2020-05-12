module Clase05
where
import Clase03


-- | Funcion productoria.
prod :: Int -> Int -> Int
prod d h | d == h =d
         | otherwise = h * prod d (h-1)

-- | Funcion productoria
prod' :: Int -> Int -> Int
prod' d h | d == h =d
          | otherwise = d * prod' (d+1) h

-- Función factorial llana
fact :: Int -> Int
fact 1 = 1
fact n = n * (fact(n-1))

-- Función factorial que restringe a productoria en 1 n
fact' :: Int -> Int
fact' n = prod 1 n

-- Ejercicio 1
sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n k | k == 1 =1
                       | (n `mod` k == 0) = k + sumaDivisoresHasta n (k-1)
                       | otherwise        = sumaDivisoresHasta n (k-1)

-- Ejercicio 2
sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n

-- Otra resolución del ej. 2
-- Función auxiliar con término k creciente y caso base n.
sumaDivisoresDesde :: Int -> Int -> Int
sumaDivisoresDesde n k | k == n =n
                        | (n `mod` k == 0) = k + sumaDivisoresDesde n (k+1)
                        | otherwise        = sumaDivisoresDesde n (k+1)

-- Si k es 1, luego se suman todos los divisores DESDE 1 HASTA N
sumaDivisores' :: Int -> Int
sumaDivisores' n = sumaDivisoresDesde n 1

-- Ejercicio 3
menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | (n `mod` k == 0) =k
                      | otherwise        = menorDivisorDesde n (k+1)

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

-- Ejercicio 4
-- esPrimo: n es primo si el menor divisor de n es, precisamente, n.
esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n

-- Otra forma de esPrimo
-- Es primo si es mayor a 1 y no tiene divisores partiendo de 2.
esPrimo' :: Int -> Bool
esPrimo' n = (n > 1) && not (tieneDivisoresDesde n 2)

tieneDivisoresDesde :: Int -> Int -> Bool
tieneDivisoresDesde n k | k == n    = False
                        | otherwise = (n `mod` k == 0) || tieneDivisoresDesde n (k+1)

-- Ejercicio 5
-- El primer primo es el 2. Luego, la función buscará el mínimo natural > Primo (n-1) que sea primo
nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n - 1))

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n + 1)

-- Ejercicio 6
menorFactDesde :: Int -> Int
menorFactDesde m = menorFactDesdeDesde 1 m

menorFactDesdeDesde :: Int -> Int -> Int
menorFactDesdeDesde i m | (fact i) >= m = fact i
                        | otherwise     = menorFactDesdeDesde (i + 1) m

-- Ejercicio 7
--
mayorFactHasta :: Int -> Int
mayorFactHasta m = mayorFactHastaHasta m m

mayorFactHastaHasta :: Int -> Int -> Int
mayorFactHastaHasta i m | (fact i) >= m = fact (i-1)
                        | otherwise     = mayorFactHastaHasta (i + 1) m

-- Ejercicio 8
-- | La función esFact determina si un número es factorial (True) o no (False).
esFact :: Int -> Bool
esFact 0 = False
esFact n | (n == menorFactDesde n) =True
         | otherwise = False

-- Ejercicio 9
esFibonacci :: Int -> Bool
esFibonacci 0 = False
esFibonacci n | (esFiboHasta 1 n) == n

-- esFiboHasta :: Int -> Int -> Int
-- esFiboHasta i m | m > i =
--                 | (fibo i) == m = fibo i

fibo


-- Ejercicio 10
-- Ejercicio 11
-- Ejercicio 12
-- Ejercicio 13
-- Ejercicio 14
-- Ejercicio 15
-- Ejercicio 16
-- Ejercicio 17
