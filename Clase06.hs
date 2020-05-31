sumatoria :: [Int] -> Int
sumatoria l | l == [] = 0
            | otherwise = head l + sumatoria (tail l)

longitud :: [Int] -> Int
longitud l | l == [] = 0
           | otherwise = 1 + longitud (tail l)

pertenece :: Int -> [Int] -> Bool
pertenece x l | l == [] = False
              | otherwise = (x == head l) || pertenece x (tail l)


primerMultiplode45345 :: [Int] -> Int
primerMultiplode45345 l | mod (head l) 45345 == 0 = (head l)
                        | otherwise = primerMultiplode45345 (tail l)

sumatoriaPM :: [Int] -> Int
sumatoriaPM [] = 0
sumatoriaPM (x:xs) = x + sumatoriaPM xs

-- 1 productoria
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- 2 sumarN
sumarN :: Int -> [Int] -> [Int]
sumarN _ []     = []
sumarN n (x:xs) = (n+x):(sumarN n xs)

-- 3 sumarElPrimero
sumarElPrimero :: [Int] -> [Int]
sumarElPrimero []     = []
sumarElPrimero (x:xs) = sumarN x (x:xs)

-- 4 sumarElUltimo
elUltimo :: [Int] -> Int
elUltimo [a]    = a
elUltimo (x:xs) = elUltimo xs

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo ls = sumarN (elUltimo ls) ls

-- 5 pares
pares :: [Int] -> [Int]
pares []     = []
pares (x:xs) | (x `mod` 2 == 0) = x:(pares xs)
             | otherwise        = pares xs

-- 6 quitar
quitar :: Int -> [Int] -> [Int]
quitar _ []     = []
quitar n (x:xs) | n == x    = xs
                | otherwise = x:(quitar n xs)

-- 7 quitarTodas
quitarTodas :: Int -> [Int] -> [Int]
quitarTodas _ []     = []
quitarTodas n (x:xs) | n == x    = (quitarTodas n xs)
                     | otherwise = x:(quitarTodas n xs)

-- 8 hayRepetidos
hayRepetidos :: [Int] -> Bool
hayRepetidos []     = False
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise      = hayRepetidos xs

-- 9 eliminarRepetidosAlFinal
eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal []     = []
eliminarRepetidosAlFinal (x:xs) = x: eliminarRepetidosAlFinal (quitarTodas x xs)

-- 10 eliminarRepetidosAlInicio
eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio []     = []
eliminarRepetidosAlInicio (x:xs) | pertenece x xs = eliminarRepetidosAlInicio xs
                                 | otherwise = x: (eliminarRepetidosAlInicio xs)

-- 11 máximo
maximo :: [Int] -> Int
maximo [a]        = a
maximo (x:(y:xs)) | x >= y    = maximo (x:xs)
                  | otherwise = maximo (y:xs)

-- 12 ordenar
ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar ls = minimo (ls) : ordenar (quitar (minimo ls) ls)

minimo :: [Int] -> Int
minimo [a]        = a
minimo (x:(y:xs)) | x <= y    = minimo (x:xs)
                  | otherwise = minimo (y:xs)

-- 13 reverso (tiene error)
reverso :: [Int] -> [Int]
reverso [] = []
reverso ls = elUltimo ls : reverso (quitar (elUltimo ls) ls)

-- 14 concatenar ...

-- 15 zapi ...