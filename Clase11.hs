type Complejo = (Float,Float)

re :: Complejo -> Float
re (a,_) = a

im :: Complejo -> Float
im (_,b) = b

conjugado :: Complejo -> Complejo
conjugado (a,b) = (a,-b)

suma :: Complejo -> Complejo -> Complejo
suma (a,b) (c,d) = (a+c,b+d)

resta :: Complejo -> Complejo -> Complejo
resta (a, b) (c, d) = (a-c,b-d)

producto :: Complejo -> Complejo -> Complejo
producto (a,b) (c,d) = (a*c - b*d, a*d + b*c)

inverso :: Complejo -> Complejo
inverso (a,b) = (a/(a**2 + b**2),(-b)/(a**2 + b**2))

-- Ejercicios --
cociente :: Complejo -> Complejo -> Complejo
cociente (a, b) (c, d) = ( (a*c + b*d) / (c**2+d**2) , (b*c - a*d) / (c**2 + d**2) )

potencia :: Complejo -> Integer -> Complejo
potencia z 1      = z
potencia (a, b) n = producto (a,b) (potencia (a,b) (n-1))

solucionesCuadratica :: Float -> Float -> Float -> (Complejo, Complejo)
solucionesCuadratica a b c | haySolucionesEnR = ( ( n + cerosReales, 0), (n - cerosReales, 0) )
                           | otherwise  = ( (n, cerosImaginarios ) , (n , -cerosImaginarios) )
      where haySolucionesEnR = (b**2 - 4*a*c >= 0)
            n = (-b/(2*a))
            cerosReales = ((sqrt(b**2 - 4*a*c))/(2*a))
            cerosImaginarios = ((sqrt(4*a*c - b**2))/(2*a))
-- -- -- -- -- --

modulo :: Complejo -> Float
modulo (a,b) = sqrt(a**2 + b**2)

argumento :: Complejo -> Float
argumento (a,b) |cuadrante (a,b) == 1 = atan (b/a)
                |cuadrante (a,b) == 2 = pi + (atan (b/a))
                |cuadrante (a,b) == 3 = pi + (atan (b/a))
                |otherwise = 2*pi + (atan (b/a))

cuadrante :: Complejo -> Int
cuadrante (a,b) |a>=0 && b>=0 = 1
                |a<=0 && b>=0 = 2
                |a<=0 && b<=0 = 3
                |a>=0 && b<=0 = 4

pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r tita = (r*(cos tita),r*(sin tita))

raizCuadrada :: Complejo -> (Complejo,Complejo)
raizCuadrada z=(pasarACartesianas r tita ,pasarACartesianas r (tita+pi))
                  where r = sqrt (modulo z)
                        tita = (argumento z)/2

-- Ejercicio
solucionesCuadraticaCoefComplejos :: Complejo -> Complejo -> Complejo -> (Complejo, Complejo)
solucionesCuadraticaCoefComplejos c w z = (( wInverso `suma` raizCompleja), wInverso `resta` raizCompleja)
      where wInverso = (-re w,-im w) `cociente` (productoRealComplejo 2 c)
            raizPositiva (a,b) = a
            raizCompleja = raizPositiva(raizCuadrada( (w `potencia` 2) `resta` ( productoRealComplejo 4 ( c `producto` z )))) `cociente` (productoRealComplejo 2 c)

productoRealComplejo :: Float -> Complejo -> Complejo
productoRealComplejo n (a,b) = (n*a,n*b)
--

raicesNEsimas :: Integer -> [Complejo]
raicesNEsimas n = raicesNEsimasDesde 0 n

raicesNEsimasDesde :: Integer -> Integer -> [Complejo]
raicesNEsimasDesde k n | k>= n = []
                       |otherwise =(kesimaRaiz):(raicesNEsimasDesde (k+1) n)
    where kesimaRaiz =(cos (2*(fromInteger k)*pi/(fromInteger n)) , sin ((2*(fromInteger k)*pi)/(fromInteger n)))

-- Ejercicio
-- Dados k y n enteros con 0 ≤ k < n, devuelve la lista con las potencias 0, 1, . . . , n − 1 de la raız
-- n-esima asociada a k siguiendo la formula de arriba.
potenciasRaizNEsima :: Integer -> Integer -> [Complejo]
potenciasRaizNEsima k 0 = []
potenciasRaizNEsima k n = (potencia (kesimaRaiz) (n-1)) : potenciasRaizNEsima k (n-1)
  where kesimaRaiz =(cos (2*(fromInteger k)*pi/(fromInteger n)) , sin ((2*(fromInteger k)*pi)/(fromInteger n)))
