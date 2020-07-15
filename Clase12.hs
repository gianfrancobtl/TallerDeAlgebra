module Clase12
where
import Clase07
--
mcd :: Int -> Int -> Int
mcd a 0 = abs a
mcd a b = mcd b (mod a b)
--
-- Primera parte

-- Defino tipos:
type Polinomio = [Float]
type Monomio = (Float, Int)

-- 1
-- Dada una lista cualquiera de n´umeros reales, le borra los 0s iniciales si los hubiera
-- (luego el resultado cumple el invariante del tipo Polinomio).
limpiar :: [Float] -> Polinomio
limpiar (0:xs) = limpiar xs
limpiar p = p

-- Dado un polinomio, devuelve su grado.
grado :: Polinomio -> Int
grado [] = undefined
grado [x] = 0
grado (x:xs) = 1 + grado xs

-- Dados un polinomio P(x) y un real a, calcula el valor de P(a)
evaluar :: Polinomio -> Float -> Float
evaluar [] _ = 0
evaluar p x = ((head p) * (x^n)) + evaluar (limpiar (tail p)) x
    where n = grado p

-- 2
sumaAux :: Polinomio -> Polinomio -> [Float]
sumaAux [] p = p
sumaAux p [] = p
sumaAux p q = sumaAux (init p) (init q) ++ [(last p) + (last q)]

suma :: Polinomio -> Polinomio -> Polinomio
suma p q = limpiar (sumaAux p q)

-- 3
-- Calcula el producto de un escalar por un polinomio.
productoPorEscalar :: Float -> Polinomio -> Polinomio
productoPorEscalar 0 _ = []
productoPorEscalar x [] = []
productoPorEscalar x p = (x * head(p)) : (productoPorEscalar x (tail p))

-- Calcula la resta de polinomios.
resta :: Polinomio -> Polinomio -> Polinomio
resta p q = suma p (productoPorEscalar (-1) q)

-- Calcula el producto de un monomio por un polinomio.
productoPorMonomio :: Monomio -> Polinomio -> Polinomio
productoPorMonomio (a, 0) p = productoPorEscalar a p
productoPorMonomio (a, n) p = (productoPorMonomio (a, (n-1)) p) ++ [0]

--  calcula el producto de dos polinomios.
producto :: Polinomio -> Polinomio -> Polinomio
producto [] q = []
producto p q = suma (productoPorMonomio (head p, grado p) q)  (producto (tail p) q)

-- 4
ceros :: Int -> [Float]
ceros 0 = []
ceros n = 0: (ceros (n-1))

-- “Convierte” un monomio en un polinomio (considerando sus respectivas codificaciones).
hacerPolinomio :: Monomio -> Polinomio
hacerPolinomio (a, n) = a : (ceros n)

-- Deriva un monomio.
derivadaMonomio :: Monomio -> Monomio
derivadaMonomio (a, n) = (a*fromIntegral(n), n-1)

-- Deriva un polinomio.
derivada :: Polinomio -> Polinomio
derivada p | (n == 0.0) = []
           | otherwise = ((head p) * n) : derivada (tail p)
    where n = fromIntegral(grado p)

derivadaNEsima :: Polinomio -> Int -> Polinomio
derivadaNEsima [] _ = []
derivadaNEsima p 1  = derivada p
derivadaNEsima p n  = derivadaNEsima (derivada p) (n-1)

-- 5
-- Calcula el primer monomio del cociente de la division de dos polinomios.
primerCociente :: Polinomio -> Polinomio -> Monomio
primerCociente p q | grado p >= grado q = (head p / head q, grado p - grado q)

-- Calcula la resta entre el dividendo y el producto del divisor por el primer cociente.
primerResto :: Polinomio -> Polinomio -> Polinomio
primerResto p q | grado p >= grado q = resta p (productoPorMonomio (primerCociente p q) q)

-- Dados dos polinomios, calcula el cociente y el resto que se obtienen al dividirlos.
division :: Polinomio -> Polinomio -> (Polinomio, Polinomio)
division p [] = undefined
division [] q = ([],[])
division p  q | grado p < grado q = ([], p)
              | otherwise = (suma (hacerPolinomio (primerCociente p q)) c', r')
          where (c', r') = division (primerResto p q) q

-- 6
mcdPNM :: Polinomio -> Polinomio -> Polinomio
mcdPNM p [] = p
mcdPNM p q  = mcdPNM q (snd (division p q)) -- llamada recursiva con q y el resto (snd) de p/q.

hacerMonico :: Polinomio -> Polinomio
hacerMonico p | p /= [] = productoPorEscalar (1 / (head p)) p

-- El MCD entre dos polinomios por Euclides es el MCD no mónico hecho mónico.
mcdP :: Polinomio -> Polinomio -> Polinomio
mcdP p q = hacerMonico (mcdPNM p q)

-- Dados un real x y un polinomio P(X) determine la multiplicidad de x como raíz de P.
multiplicidad :: Float -> Polinomio -> Int
multiplicidad n p = multiplicidad' [1,-n] p 1

multiplicidad' :: Polinomio -> Polinomio -> Int -> Int
multiplicidad' q p n | snd (division p (potencia q n)) /= [] = 0
                     | otherwise = 1 + multiplicidad' q p (n+1)

potencia :: Polinomio -> Int -> Polinomio
potencia p 1 = p
potencia p n = producto p (potencia p (n-1))

-- Otra opcion, sin contador.
multiplicidad2 :: Float -> Polinomio -> Int
multiplicidad2 r p | resto /= [] = 0
                   | resto == [] = 1 + multiplicidad2 r cociente
    where (cociente, resto) = division p [1,-r]

raicesMultiples :: Polinomio -> Bool
raicesMultiples p = (p `mcdP` derivada p) /= [1]

-- Segunda Parte
-- Definimos un renombre de tipos para representar a los números racionales, dado que
-- queremos poner el énfasis en la expresión de estos n´umeros como cociente entre dos enteros.
type Racional = (Int, Int)

-- 7
-- Dados un candidato a numerador y un candidato a denominador, devuelve el racional.
armaR :: Int -> Int -> Racional
armaR num den | den == 0 = undefined
              | den < 0 = armaR (-num) (-den)
              | otherwise = (div num d, div den d)
  where d = mcd num den

-- Suma de dos racionales.
sumaR :: Racional -> Racional -> Racional
sumaR (a, b) (c, d) = armaR (a*d+b*c) (b*d)

-- Multiplicación de dos racionales.
multiplicaR :: Racional -> Racional -> Racional
multiplicaR (a, b) (c, d) = armaR (a*c) (b*d)

-- Potencia de dos racionales.
potenciaR :: Racional -> Int -> Racional
potenciaR _ 0 = (1, 1)
potenciaR r n = multiplicaR (potenciaR r (n-1)) r

-- 8
-- Para trabajar con polinomios en Z[X], definimos un renombre de tipos:
type PolinomioZ = [Int]

-- Funciones auxiliares
limpiarZ :: [Int] -> PolinomioZ
limpiarZ (0:xs) = limpiarZ xs
limpiarZ p = p

gradoZ :: PolinomioZ -> Int
gradoZ [] = undefined
gradoZ [x] = 0
gradoZ (x:xs) = 1 + gradoZ xs
--

-- Evalua el Polinomio en un número racional.
evaluarZ :: PolinomioZ -> Racional -> Racional
evaluarZ [] _ = (0, 1)
evaluarZ p x  = sumaR (multiplicaR (head p, 1) (potenciaR x n))  (evaluarZ (limpiarZ (tail p)) x)
    where n = gradoZ p

--
esRaizRacional :: PolinomioZ -> Racional -> Bool
esRaizRacional p r = evaluarZ p r == (0,1)

raicesRacEnConjunto :: PolinomioZ -> Set Racional -> Set Racional
raicesRacEnConjunto p [] = []
raicesRacEnConjunto p (x:xs) | esRaizRacional p x = x : raicesRacEnConjunto p xs
                             | otherwise = raicesRacEnConjunto p xs

-- 9
-- Encuentra todos los divisores de un n.
divisores :: Int -> Set Int
divisores n = divisoresHasta n (abs n)

divisoresHasta :: Int -> Int -> Set Int
divisoresHasta n 1 = [1, -1]
divisoresHasta n k | mod n k == 0 = [k, -k] ++ divisoresHasta n (k-1)
                   | otherwise = divisoresHasta n (k-1)

-- Encuentra todos los divisores positivos de un n.
divisoresPos :: Int -> Set Int
divisoresPos n = divisoresPosHasta n (abs n)

divisoresPosHasta :: Int -> Int -> Set Int
divisoresPosHasta n 1 = [1]
divisoresPosHasta n k | mod n k == 0 = k:divisoresPosHasta n (k-1)
                      | otherwise = divisoresPosHasta n (k-1)

agregarRac :: Racional -> Set Racional -> Set Racional
agregarRac r c | elem r c = c
               | otherwise = r:c

armarSetRac :: Set (Int, Int) -> Set Racional
armarSetRac [] = []
armarSetRac ((num, den):xs) = agregarRac (armaR num den) (armarSetRac xs)

-- Basándose en el teorema de Gauss, devuelve los candidatos a raíces racionales de p.
candidatosRaices :: PolinomioZ -> Set Racional
candidatosRaices [] = undefined
candidatosRaices p | last p == 0 = agregarRac (0,1) (candidatosRaices (init p))
                   | otherwise = armarSetRac (productoCartesiano (divisores (last p)) (divisoresPos (head p)))

-- Devuelve las racíces racionales del polinomio.
raicesRacionales :: PolinomioZ -> Set Racional
raicesRacionales p = raicesRacEnConjunto p (candidatosRaices p)
