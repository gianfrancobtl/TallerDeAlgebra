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
union [] c2      = c2
union (x:xs) c2 = union xs (agregar x c2)

interseccion :: Set Int -> Set Int -> Set Int
interseccion [] _ = []
interseccion (x:xs) c2 | pertenece x c2 = x : (interseccion xs c2)
                       | otherwise = (interseccion xs c2)

diferencia :: Set Int -> Set Int -> Set Int
diferencia 