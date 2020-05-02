module Clase04
where

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