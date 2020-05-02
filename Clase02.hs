module Clase02
where
{-| La funcion estanRelacionados decide si dos números reales x y
 están relacionados considerando la relación de equivalencia en R
 cuyas clases de equivalencia son: (−∞, 3], (3, 7] y (7, ∞) -}
estanRelacionados :: (Ord t, Floating t) => t -> t -> Bool
estanRelacionados x y | (x <= (-3)) && (y<= (-3)) =True
                      | (x > (-3) && x <= 7) && (y > (-3) && y <= 7) =True
                      | otherwise =False

-- | Otra forma de estarRelacionados con una función auxiliar.
clase:: Float -> Int
clase x | x <= 3 =1
        | x > 3 && x<= 7 =2
        | x > 7 =3

estanRelacionados2 :: Float -> Float -> Bool
estanRelacionados2 x y = clase x == clase y

-- | prodInt calcula el producto interno entre dos vectores de R2
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt u v = fst(u) * fst(v) + snd (u) * snd(v)

-- | Agregado de la clase virtual. Otra opción y uso de funciones auxiliares.
prodInt2 :: (Float, Float) -> (Float, Float) -> Float
prodInt2 (a,b) (x,y) = a * x + b * y

normaVectorial :: (Float, Float) -> Float
normaVectorial v = sqrt (prodInt2 v v)

resta:: (Float, Float) -> (Float, Float) -> (Float, Float)
resta (a,b) (x,y) = (a - x, b - y)

distanciaPuntos2 :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos2 u v = normaVectorial (resta u v)

{- | todoMenor  decide si es cierto que cada coordenada del primer
vector es menor a la coordenada correspondiente del segundo vector.-}
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor u v | fst(u) > fst(v) =False
              | snd(u) > snd(v) =False
              | otherwise =True

-- | La función distanciaPuntos calcula la distancia entre dos puntos de R2.
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos u v = sqrt (((fst(u) - fst(v)))^2 + (snd(u) - snd(v))^2)

-- | sumaTerna calcula la suma de los tres elementos enteros de una terna.
sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x, y, z) = x + y + z

{- | posicPrimerPar devuelve la posición del primer número par de una
terna entera si es que hay alguno, y devuelve 4 si son todos impares-}
posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (x, y, z) | esPar x =1
                         | esPar y =2
                         | esPar z =3
                         | otherwise = 4

-- | Funcion auxiliar que determina si n es par.
esPar :: Int -> Bool
esPar n = mod n 2 == 0

{- |crearPar crea un par a partir de sus dos componentes dadas por separado
(funciona para elementos de cualquier tipo) -}
crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

{- | invertir invierte los elementos del par pasado como parámetro
(funciona para elementos de cualquier tipo)-}
invertir :: (a, b) -> (b, a)
invertir (a,b) = crearPar b a
