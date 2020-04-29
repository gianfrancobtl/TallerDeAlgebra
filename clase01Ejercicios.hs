--------------------------------------------
------ Clase 01 del taller de Algebra ------
--------------------------------------------
g x y z = x + y + z*z

doble x = 2*x

suma x y = x + y

normaVectorial x1 x2 = sqrt (x1^2 + x2^2)

funcionConstante8 x = 8

f 0 = 1
f n = 0

signo 0 = 0
signo n | n > 0 =1
        | otherwise = (-1)

maximo :: Int -> Int -> Int
maximo x y | x>=y =x
           | otherwise =y

maximoRac :: Float -> Float -> Float
maximoRac x y | x>=y =x
              | otherwise =y

f1 n | n >= 3 = 5

f2 n | n >= 3 = 5
     | n <= 1 = 8

f3 n | n >= 3 = 5
     | n == 2 = undefined
     | otherwise = 8

f4 n | n >= 3 = 5
     | n <= 9 = 7

cantidadDeSoluciones b c | d > 0 =2
                         | d == 0 =1
                         | otherwise = 0
                         where d = b^2 - 4*c

esMayorA9 :: Int -> Bool
esMayorA9 n | n > 9 = True
            | otherwise = False

esPar :: Int -> Bool
esPar n | mod n 2 == 0 = True
        | otherwise = False

esPar2 :: Int -> Bool
esPar2 n = mod n 2 == 0

esImpar :: Int -> Bool
esImpar n = not (esPar n)

funcionRara :: Float -> Float -> Bool -> Bool
funcionRara _ _ True = True
funcionRara x y False = x >= y

--------------------------------------------
-------------- Actividades -----------------
--------------------------------------------

absoluto :: Int -> Int
absoluto n | n > 0 =n
           | n < 0 =n*(-1)

maximoabsoluto :: Int -> Int -> Int
maximoabsoluto a b = (maximo(absoluto a) (absoluto b))

maximo3 :: Int -> Int -> Int -> Int
maximo3 a b c = (maximo d c)
        where d = maximo a b

algunoEs0 :: Float -> Float -> Bool
algunoEs0 a b | (a == 0) || (b == 0) =True
              | otherwise =False

aE0PatternMatching :: Float -> Float -> Bool
aE0PatternMatching _ 0 =True
aE0PatternMatching 0 _ =True
aE0PatternMatching _ _ =False

ambosSon0 :: Float -> Float -> Bool
ambosSon0 a b | (a == 0) && (b == 0) =True
              | otherwise =False

aS0PatternMatching :: Float -> Float -> Bool
aS0PatternMatching 0 0 =True
aS0PatternMatching _ _ =False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b | (a <= 0) || (b <= 0) = undefined
                 | mod a b == 0 =True
                 | otherwise =False

digitoUnidades :: Int -> Int
digitoUnidades n = mod n 10

digitoDecenas :: Int -> Int
digitoDecenas n = digitoUnidades (div n 10)
