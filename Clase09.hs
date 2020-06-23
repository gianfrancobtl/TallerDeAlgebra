module Clase09
where
import Clase07
import Clase06

-- Algoritmo de la división
-- | Division de números naturales_0: a `divNat` d = a `div` d
divNat :: Int -> Int -> Int
divNat a d | a < d = 0
           | otherwise = ((a-d) `divNat` d) + 1

-- | Resto de numeros naturales_0: a `modNat` d = a `mod` d
modNat :: Int -> Int -> Int
modNat a d = a - d*(a `divNat` d)

-- | Módulo de numeros enteros : a `modulo` d = a `mod` d
modulo :: Int -> Int -> Int
modulo a d | a >= 0 || r' == 0 = r'
           | otherwise = abs d - r'
    where r' = abs a `modNat` abs d

-- | División de numeros enteros: n `dividido` m = n `div` m
dividido :: Int -> Int -> Int
dividido a d = sgq * absq
    where absq = abs(a-r) `divNat` (abs d)
          sgq = (signum a) * (signum d)
          r = a `modulo` d

-- Sistemas de numeración
-- ej 1
digitos :: Int -> Int -> [Int]
digitos 0 _ = []
digitos n b = n `modulo` b : digitos (n `dividido` b) b

-- ej 2
numero :: [Int] -> Int -> Int
numero [a] _ = a
numero l  n = (last l)*n^j + numero (init l) n
    where j = length(l)-1

-- MCD
-- Posible algoritmo por def:
mcdDef :: Int -> Int ->  Int
mcdDef a 0 = abs a
mcdDef 0 b = abs b
mcdDef a b = maximo (interseccion (divisores a) (divisores b))

divisores :: Int -> Set Int
divisores n = listaDivisores n n

listaDivisores :: Int -> Int -> Set Int
listaDivisores n 0 = []
listaDivisores n m | n `mod` m == 0 = m:listaDivisores n (m-1)
                   | otherwise      = listaDivisores n (m-1)

-- Algoritmo de Euclides
mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

-- minimo comun múltiplo
mcm :: Int -> Int -> Int
mcm 0 0 = 0
mcm a b = round((fromIntegral(a * b)) / (fromIntegral(mcd a b)))

mcm' :: Int -> Int -> Int
mcm' 0 0 = 0
mcm' a b = a * b `div` (mcd a b)

-- ej 1
emcd :: Int -> Int -> (Int, Int, Int)
emcd a b = (mcd a b):(generaSyT a b)

-- generaSyT :: Int -> Int -> (Int, Int)
-- generaSyT a b = 
