-- TRABAJO PRÁCTICO 1

-- EJ 1: sonCoprimos.
-- Dados dos números naturales, decide si son coprimos.
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b | (a == b)  = False
                | otherwise = not (sonDivisiblesPor 2 a b)

-- AUX: Dados a y b, indica si son divisibles por un n que va incrementándose hasta ser mayor que a o b.
sonDivisiblesPor :: Integer -> Integer -> Integer -> Bool
sonDivisiblesPor n a b | (n > a || n > b)                     = False
                       | (a `mod` n == 0) && (b `mod` n == 0) = True
                       | otherwise                            = sonDivisiblesPor (n + 1) a b

-----
-- EJ 2: es2Pseudoprimo.
-- Dado un número natural decide si es 2-pseudoprimo.
es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo p = esAPseudoprimo 2 p

-- AUX 1: Dados a y p, la función decide si p es un a-pseudoprimo.
-- Para esto: a^(p-1)-1 debe ser divisible por p; p no debe ser primo.
esAPseudoprimo :: Integer -> Integer -> Bool
esAPseudoprimo a p = ((a^(p-1) - 1) `mod` p == 0) && not (esPrimo p)

-- AUX 2: Dado un número natural, decide si es primo.
esPrimo :: Integer -> Bool
esPrimo n = (n > 1) && not (tieneDivisoresDesde n 2)

-- AUX 2': Función auxiliar de esPrimo.
tieneDivisoresDesde :: Integer -> Integer -> Bool
tieneDivisoresDesde n k | k == n    = False
                        | otherwise = (n `mod` k == 0) || tieneDivisoresDesde n (k+1)

-----
-- EJ 3: cantidad3Pseudoprimos.
-- Dado un número natural m calcula la cantidad de 3-pseudoprimos que hay entre 1 y m inclusive.
cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos m = cant3PseudoprimosHasta m 90

-- AUX 1: Dado un número natural decide si es 3-pseudoprimo.
cant3PseudoprimosHasta :: Integer -> Integer -> Integer
cant3PseudoprimosHasta m n | m <= n           = 0
                           | es3Pseudoprimo m = 1 + cant3PseudoprimosHasta (m - 1) n
                           | otherwise        = cant3PseudoprimosHasta (m - 1) n

-- AUX 2: Dado un número natural decide si es 3-pseudoprimo.
es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo p = esAPseudoprimo 3 p

-----
-- EJ 4: kesimo2y3Pseudoprimo.
-- Dado un número natural k calcula el k-ésimo número que es simultáneamente 2-pseudoprimo y 3-pseudoprimo.
kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo 1 = 1105
kesimo2y3Pseudoprimo k = minimoPseudoprimoDesde (1 + kesimo2y3Pseudoprimo (k - 1))

-- AUX: Dado n (último 2-pseudoprimo y 3-pseudoprimo + 1), encuentra el siguiente que cumpla las condiciones.
minimoPseudoprimoDesde :: Integer -> Integer 
minimoPseudoprimoDesde n | es2Pseudoprimo n && es3Pseudoprimo n = n
                         | otherwise                            = minimoPseudoprimoDesde (n + 1)

-----
-- EJ 5: esCarmichael.
-- Dado un número natural decide si es un número de Carmichael.
esCarmichael :: Integer -> Bool
esCarmichael n = esCarmichaelHasta (n-1) n

-- AUX: Dados a y n, indica si son a-pseudoprimos en caso que sean coprimos.
esCarmichaelHasta :: Integer -> Integer -> Bool
esCarmichaelHasta a n | (a == 1) && not (n == 2)                   = True
                      | sonCoprimos a n && not(esAPseudoprimo a n) = False
                      | otherwise                                  = esCarmichaelHasta (a-1) n
