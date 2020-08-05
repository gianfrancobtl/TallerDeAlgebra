-- Trabajo práctico. Sistema criptográfico RSA. 

-- Librería que permite utilizar ord y chr, 
-- que dan una correspondencia entre caracteres y números enteros entre 0 y 127.
import Data.Char (ord, chr)

-- EJ 1: GENERAR CLAVES
-- Dados dos numeros primos p y q, genera un par que contiene una clave publica y una clave privada en
-- el formato ((n, d),(n, e))
generarClaves :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer))
generarClaves p q | ( n <= 127) = undefined      -- Si n < 127, la función esta indefinida.     
                  | otherwise = ( (n,d), (n,e) ) -- Sino, devuelve la clave pública y la clave privada.
    where n = p * q
          m = (p-1) * (q-1)
          e = minimoCoprimoE m 2 
          d = fromIntegral(fst (solucionEc (fromInteger(e), 1, fromInteger(m)) ) )
          -- d se obtiene de resolver la diofántica eX `congruente` 1 (mod m)

-- -- --
-- Funciones auxiliares del Ej. 1:
-- AUX 1: La siguiente, encuentra el mínimo primo "e"/ 2 <= e <= m-2:
minimoCoprimoE :: Integer -> Integer -> Integer
minimoCoprimoE m i | (sonCoprimos m i) && (i <= (m - 2)) = i -- si m i son coprimos y el i es menor a m-2, devuelve i.
                   | otherwise = minimoCoprimoE m (i + 1)    -- sino, prueba con m (i + 1)

-- AUX 1.1: La siguiente, decide si dos números son coprimos:
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b | (a == b)  = False
                | otherwise = not (sonDivisiblesPor 2 a b)

-- AUX 1.1.1: La siguiente es una función auxiliar de sonCoprimos.
sonDivisiblesPor :: Integer -> Integer -> Integer -> Bool
sonDivisiblesPor n a b | (n > a || n > b)                     = False
                       | (a `mod` n == 0) && (b `mod` n == 0) = True
                       | otherwise                            = sonDivisiblesPor (n + 1) a b

-- AUX 2: Brinda una solucion simplificada, con a = 1.
solucionEc :: (Int, Int, Int) -> (Int, Int)
solucionEc e = solucionEcConPropAdic (ecEquivalente e)

-- AUX 2.1: Si paso (7, 2, 25) --> (11, 25)
solucionEcConPropAdic :: (Int, Int, Int) -> (Int, Int)
solucionEcConPropAdic (a, b, m) = (mod (s*b) m, m)
      where (d, s, t) = emcd a m

-- AUX 2.2:Toma la ecuacion dada y devuelve otra donde a y m son coprimos.
-- Si paso (7, 2, 25) --> idem.
-- Si paso (2, 4, 6)  --> (1, 2, 3)
-- Si no tiene solucion --> undefined
ecEquivalente :: (Int, Int, Int) -> (Int, Int, Int)
ecEquivalente (a, b, m) | mod b d /= 0  = undefined
                        | otherwise = (div a d, div b d, div m d)
      where d = mcd a m

-- AUX 3: Dados dos números, devuelve el algoritmo de Euclides extendido
-- Es decir, (MCD, s, t), siendo s y t / s*a -t*b = mcd.
emcd :: Int -> Int -> (Int, Int, Int)
emcd a 0 = (abs a, signum a, 0)
emcd a b = (mcd, tau, sigma - (a `div` b) * tau)
    where (mcd, sigma, tau) = (emcd b (a `mod` b))

-- AUX 4: MCD con el Algoritmo de Euclides
mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b = mcd b (a `mod` b)
-- -- --

-- EJ 2: ENCRIPTAR
-- Dada una clave pública y un mensaje, lo reemplaza por la lista de enteros que lo encripta.
encriptar :: (Integer, Integer) -> String -> [Integer]
encriptar pubK [] = []
encriptar (n,d) msg = a^d `mod` n : encriptar (n,d) (tail msg)
      where a = fromIntegral (ord (head msg) )

-- EJ 3: DESENCRIPTAR
-- Dada una clave privada y una lista de enteros que encripta un mensaje, lo desencripta.
desencriptar :: (Integer, Integer) -> [Integer] -> String
desencriptar privK []  = []
desencriptar (n,e) msg = chr ( fromInteger( b^e `mod` n ) ) : desencriptar (n,e) (tail msg)
      where b = head msg

-- EJ. OPCIONAL: Desencripta un mensaje dada su clave pública.
romperCodigo :: (Integer, Integer) -> [Integer] -> String
romperCodigo pubK msg = desencriptar (n,e) msg
      where n = fst (pubK)
            e = snd (encontrarPrivKey pubK) 

{-| 
El mensaje
[33706, 38913, 58255, 99961, 77756, 23881, 220, 77756, 1606, 38913, 77756, 78982, 18800,
91658, 91658, 58255, 77756, 96593, 58255, 438, 22839, 28700, 18800, 1606, 58255, 48389]
significa "Cuál es tu pizza favorita?"

Mi respuesta es:
[78387,58255,77756,96986,58255,78982,22839,99961,18800,1606,58255,96986,58255,31608]
Clave pública: (100337, 60953)
-}

-- -- --
-- Funciones auxiliares de romperCodigo:
-- AUX 1: Encuentra la clave privada (n,e) dada una clave pública (n,d).
encontrarPrivKey :: (Integer, Integer) -> (Integer, Integer)
encontrarPrivKey (n,d) = (n,e)
      where m = (fst (factorizaN n 1) - 1) * ( snd (factorizaN n 1) - 1 )
            e = (encuentraElE d m 1)

-- AUX 1.1: Encuentra el "e" de la clave privada.
encuentraElE :: Integer -> Integer -> Integer -> Integer 
encuentraElE d m e | (d*e `mod` m == 1) = e
                   | otherwise = encuentraElE d m (e + 1)

-- AUX 1.2: Dado n , lo factoriza en p y q (primos).
factorizaN :: Integer -> Integer -> (Integer, Integer)
factorizaN n i | n `mod` i == 0 && (esPrimo i) = (i, n `div` i) 
               | otherwise = factorizaN n (i + 1)

-- AUX 1.2.1: Dado un número natural, decide si es primo.
esPrimo :: Integer -> Bool
esPrimo n = (n > 1) && not (tieneDivisoresDesde n 2)

-- AUX 1.2.2: Función auxiliar de esPrimo.
tieneDivisoresDesde :: Integer -> Integer -> Bool
tieneDivisoresDesde n k | k == n    = False
                        | otherwise = (n `mod` k == 0) || tieneDivisoresDesde n (k+1)