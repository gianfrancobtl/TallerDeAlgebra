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
digitos :: Integer -> Integer -> [Integer]

numero :: [Integer] -> Integer -> Integer



