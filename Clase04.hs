module Clase04
where
import Clase03

sumatoria :: Int -> Int
sumatoria 0 = 0
sumatoria n = n + sumatoria (n-1)

sumatoria' :: Int -> Int
sumatoria' n = n * (n + 1) `div` 2

f1 :: Int -> Int
f1 0 = 1
f1 n = 2^n + f1(n-1)

f1' :: Int -> Int
f1' n = 2^(n + 1) - 1

f2 :: Int -> Float -> Float
f2 0 q = 0
f2 n q = q^n + f2 (n-1) q

f3 :: Int -> Float -> Float
f3 0 q = 0
f3 n q = (f3 (n-1) q) + q^(2*n-1) + q^(2*n)

f3' :: Int -> Float -> Float
f3' n q = f2 (2*n) q

f4 :: Int -> Float -> Float
f4 0 q = 1
f4 n q = q^(2*n-1) + q^(2*n) - q^(n-1) + (f4 (n-1) q)

f4' :: Int -> Float -> Float
f4' n q = (f3 n q) - (f2 (n-1) q)

-- Funcion auxiliar factorial.
fact :: Int -> Int
fact 1 = 1
fact n = n * (fact(n-1))

-- | Aproximación a e.
eAprox :: Int -> Float
eAprox 0 = 1
eAprox n = (eAprox(n-1)) + 1 / (fromIntegral (fact n))

-- | Número e
e :: Float
e = eAprox 10

-- Se define f...
f6 :: Int -> Int -> Int
f6 0 m = 0
f6 n m = (f6 (n-1) m) + round (f2 m (fromIntegral n))

sumaPotencias :: Float -> Int -> Int -> Float
sumaPotencias q n 0 = 0
sumaPotencias q n m = (sumaPotencias q n (m-1)) + q^m * (f2 n q)

sumaRacionales ::  Int -> Int -> Float
sumaRacionales n 0 = 0
sumaRacionales n m = (sumaRacionales n (m-1)) + (fromIntegral(sumatoria n)) / (fromIntegral m)

-- EJERCICIOS DE TAREA --
-- EJERCICIO 4 --
g1 :: Int -> Int -> Int
g1 i n | n < i =0
       | otherwise = i^n + g1 i (n-1)

-- EJERCICIO 5 --
-- suma i^n dssde i=1 hasta n (g1 "al revés").
hAzul :: Int -> Int -> Int
hAzul 1 n = 1
hAzul i n = i^n + hAzul (i-1) n

-- suma de i^j i desde 1 hasta n, j desde i hasta n.
g2 :: Int -> Int
g2 1 = 1
g2 n = g2 (n-1) + hAzul n n

-- OTRA MANERA --
-- suma desde i=1 hasta n (g1 i m)
g2Aux :: Int -> Int -> Int
g2Aux 1 m = g1 1 m
g2Aux n m = g1 n m + g2Aux (n-1) m

g2bis :: Int -> Int
g2bis n = g2Aux n n

-- EJERCICIO 6 --
-- g3 chequea que n sea par, otherwise vuelve a llamarla para n-1, es decir, con un n par.
g3 :: Int -> Int
g3 n | n `mod` 2 == 0 =g3 (n-1) + 2^n
     | otherwise = g3 (n-1)

-- EJERCICIO 7 --
g4 :: Int -> Int
g4 0 = 0
g4 n | digitosIguales n = n + g4 (n-1)
     | otherwise = g4 (n-1)
