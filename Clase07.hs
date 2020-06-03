type Set a = [a]

vacio :: Set Int
vacio = []

pertenece :: Int -> Set Int -> Bool
pertenece _ [] = False
pertenece x c = (x == head c) || pertenece x (tail c)

agregar :: Int -> Set Int ->  Set Int
agregar x c | pertenece x c = c
            | otherwise = x:c

incluido :: Set Int -> Set Int -> Bool
incluido [] _ = True
incluido (x:xs) c = pertenece x c && incluido xs c

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

union :: Set Int -> Set Int -> Set Int
union [] c2     = c2
union (x:xs) c2 = union xs (agregar x c2)

interseccion :: Set Int -> Set Int -> Set Int
interseccion [] _ = []
interseccion (x:xs) c2 | pertenece x c2 = x : (interseccion xs c2)
                       | otherwise      = (interseccion xs c2)

diferencia :: Set Int -> Set Int -> Set Int
diferencia [] _      = []
diferencia (x:xs) c2 | not (pertenece x c2) = x : diferencia xs c2
                     | otherwise            = diferencia xs c2

diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica c1 c2 = union (diferencia c1 c2) (diferencia c2 c1)

-- Funciones agregar, pertenece y union definidas para un orden superior.
agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss | xs `perteneceC` xss = xss
                | otherwise = xs:xss

perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC xs []       = False
perteneceC xs (ys:yss) = iguales xs ys || perteneceC xs yss

unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC [] c2       = c2
unionC (xs:xss) c2 = unionC xss (agregarC xs c2)

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos x []     = []
agregarATodos x (c:cs) = agregarC (agregar x c) (agregarATodos x cs)

partes :: Set Int -> Set (Set Int)
partes []     = [[]]
partes (x:xs) = unionC (partes xs) (agregarATodos x (partes xs))

-- Ejercicios
-- 1
partesN :: Int -> Set (Set Int)
partesN 0 = [[]]
partesN n = unionC (agregarATodos n (partesN (n-1))) (partesN (n-1))

-- 2
unionCartesiana :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
unionCartesiana [] c = c
unionCartesiana (x:xs) c = x : (unionCartesiana xs c)

productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano [] _ = []
productoCartesiano (x:xs) ys = unionCartesiana (prodCartAux x ys) (productoCartesiano xs ys)

prodCartAux :: Int -> Set Int -> Set (Int, Int)
prodCartAux x [] = []
prodCartAux x (y:ys) = (x,y) : prodCartAux x ys
