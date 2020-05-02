module Clase03
where
-- | funcion de tribonacci
tribonacci :: Int -> Int
tribonacci n | n <= 2 =2
             | otherwise = tribonacci (n - 1) + tribonacci (n - 2) + tribonacci (n - 3)

-- | Multiplo de 3
multiploDe3 :: Int -> Bool
multiploDe3 0 = True
multiploDe3 1 = False
multiploDe3 2 = False
multiploDe3 n = multiploDe3 (n - 3)

-- | Dado un número n, indica si todas sus cifras son 6 (True) o no (False).-
diabolico :: Int -> Bool
diabolico 0 = True
diabolico n = (n `mod` 10) == 6 && diabolico (n `div` 10)

-- | Dado un número n, indica si todas sus cifras son iguales (True) o no (False).-
digitosIguales :: Int -> Bool
digitosIguales n | n < 0 = digitosIguales (-n)
                 | n <= 9 = True
                 | otherwise = mod (n `div` 10) 10 == (n `mod` 10) && digitosIguales (n `div` 10)

-- Idem pero funciona solo para números naturales. --
digitosIguales2 :: Int -> Bool
digitosIguales2 0 = True
digitosIguales2 n | (n >= 11) =n `mod` 10 == (n `div` 10) `mod` 10 && digitosIguales2 (n `div` 10)
                  | otherwise = n == n `mod` 10 && digitosIguales2 (n `div` 10)

-- | Dados dos números enteros, decide si el primero (m) es potencia del segundo (n). -
potenciaDe :: Int -> Int -> Bool
potenciaDe 1 _ = True
potenciaDe m n = m `mod` n == 0 && potenciaDe (m `div` n) (n)

-- Otra posibilidad de PotenciaDe en una sola línea de código. -
esPotenciaDe2 :: Int -> Int -> Bool
esPotenciaDe2 n m = (n == 1) || esPotenciaDe2 (div n m) m && (mod n m == 0)
