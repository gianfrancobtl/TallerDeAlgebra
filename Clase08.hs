type Set a = [a]

-- Funciones polimóficas : se define para /= tipos de datos.
vacio :: Set a
vacio = []

pertenece :: Eq a => a -> Set a -> Bool
pertenece _ [] = False
pertenece x c = (x == head c) || pertenece x (tail c)

agregar :: Eq a => a -> Set a -> Set a
agregar x c | pertenece x c = c
            | otherwise = x:c

union :: Eq a => Set a -> Set a -> Set a
union [] c2     = c2
union (x:xs) c2 = union xs (agregar x c2)

diferencia :: Eq a => Set a -> Set a -> Set a
diferencia [] _      = []
diferencia (x:xs) c2 | not (pertenece x c2) = x : diferencia xs c2
                     | otherwise            = diferencia xs c2
-- -- -- -- --

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- 1
combinatorio :: Int -> Int -> Int
combinatorio n k = (fact n) `div` ((fact k) * fact (n-k))

-- 2
combinatorio' :: Int -> Int -> Int
combinatorio' _ 0 = 1
combinatorio' n k | n == k =1
                  | otherwise = (combinatorio' (n-1) k) + (combinatorio' (n-1) (k-1))

-- 3: Toma un conjunto c y genera todas las variaciones posibles de k elementos.
agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElementosAListas [] _ = []
agregarElementosAListas (x:xs) c = (agregarElementoAdelante x c) `union` (agregarElementosAListas xs c)

agregarElementoAdelante :: Int -> Set [Int] -> Set [Int]
agregarElementoAdelante _ [] = []
agregarElementoAdelante x (ys:yss) = (x:ys) `agregar` (agregarElementoAdelante x yss)

variaciones :: Set Int -> Int -> Set [Int]
variaciones _ 0 = [[]]
variaciones c k = agregarElementosAListas c (variaciones c (k - 1))

-- 4
insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn l n 1 = n:l
insertarEn (x:xs) n i = x : (insertarEn xs n (i-1))

-- 5 
-- Si paso [1,2] 3 3, devuelve [[1,2,3], [1,3,2], [3,1,2]]
insertarEnCadaPos :: [Int] -> Int -> Int -> Set [Int]
insertarEnCadaPos xs c 1 = (insertarEn xs c 1) `agregar` vacio
insertarEnCadaPos xs c i = (insertarEn xs c i) `agregar` (insertarEnCadaPos xs c (i-1))

insertarEnCadaPosDeTodasLasListas :: Set [Int] -> Int -> Set [Int]
insertarEnCadaPosDeTodasLasListas [] _ = []
insertarEnCadaPosDeTodasLasListas (xs:xss) c = (insertarEnCadaPos xs c (length xs + 1)) `union`
                                               (insertarEnCadaPosDeTodasLasListas xss c)

permutaciones :: Set Int -> Set [Int]
permutaciones [] = [[]]
permutaciones (c:cs) = insertarEnCadaPosDeTodasLasListas (permutaciones cs) c

-- Ejercitacion
-- 1
conjuntoCajas :: Int -> Set Int
conjuntoCajas 0 = []
conjuntoCajas k = k `agregar` conjuntoCajas (k-1)

bolitasEnCajas :: Int -> Int -> Set [Int]
bolitasEnCajas 0 _ = []
bolitasEnCajas n k = variaciones (conjuntoCajas k) n

-- otra opción
bolitasEnCajas' :: Int -> Int -> Set [Int]
bolitasEnCajas' 0 _ = []
bolitasEnCajas' n k = variaciones [1..k] n

-- 2
perteneceEl1 :: Set [Int] -> Set [Int]
perteneceEl1 [] = []
perteneceEl1 (xs:xss) | 1 `pertenece` xs = xs : (perteneceEl1 xss)
                      | otherwise = perteneceEl1 xss

bolitasConPrimeraCajaOcupada :: Int -> Int -> Set [Int]
bolitasConPrimeraCajaOcupada n k = perteneceEl1 (bolitasEnCajas n k)

-- otra forma 
bolitasConPrimeraCajaOcupada' :: Int -> Int -> Set [Int]
bolitasConPrimeraCajaOcupada' n k = insertarEnCadaPosDeTodasLasListas (bolitasEnCajas (n-1) k) 1

-- otra forma
bolitasConPrimeraCajaOcupada'' :: Int -> Int -> Set [Int]
bolitasConPrimeraCajaOcupada'' n k = (variaciones [1..k] n) `diferencia` (variaciones [2..k] n)

-- 3
