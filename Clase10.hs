module Clase10
where
import Clase05
import CodificacionDeListas
import Clase09


-- 1 ---------------------

-- Toma la ecuacion dada y devuelve otra donde a y m son coprimos.
-- Si paso (7, 2, 25) --> idem.
-- Si paso (2, 4, 6)  --> (1, 2, 3)
-- Si no tiene solucion --> undefined
ecEquivalente :: (Int, Int, Int) -> (Int, Int, Int)
ecEquivalente (a, b, m) | mod b d /= 0  = undefined
                        | otherwise = (div a d, div b d, div m d)
 where d = mcd a m

-- Si paso (7, 2, 25) --> (11, 25)
solucionEcConPropAdic :: (Int, Int, Int) -> (Int, Int)
solucionEcConPropAdic (a, b, m) = (mod (s*b) m, m)
 where (d, s, t) = emcd a m

-- Brinda una solucion simplificada, con a = 1.
solucionEc :: (Int, Int, Int) -> (Int, Int)
solucionEc e = solucionEcConPropAdic (ecEquivalente e)


-- 2 --------------------
-- Devuelve un sistema de ecuaciones simplificadas, es decir, con a = 1.
-- Genera las soluciones individuales y las va uniendo con (:)
sistemaSimplifEquiv :: [(Int, Int, Int)] -> [(Int, Int)]
sistemaSimplifEquiv [] = []
sistemaSimplifEquiv (e:es) = (solucionEc e):(sistemaSimplifEquiv es)


-- -- 3 --------------------

-- Tomo los módulos de los S.S.
-- [(11,25), (4,22)] --> [25,22]
modulos :: [(Int, Int)] -> [Int]
modulos [] = []
modulos ((r, m):es) = m:(modulos es)

-- [(11,25), (4,22)] --> [25]
mayorModulo :: [(Int, Int)] -> Int
mayorModulo sist = maximum (modulos sist)

cotaParaPrimoMaloDesde :: [(Int, Int)] -> Int -> Int
cotaParaPrimoMaloDesde sist n | nEsimoPrimo (n+1) > (mayorModulo sist) = n
                              | otherwise = cotaParaPrimoMaloDesde sist (n+1)

-- [(11,25), (4,22)] --> 9 pues hay 9 primos malos posibles (2, 3, 5, 7, 11, 13, 17, 19, 23)
-- menores al mayor módulo del sistema.
cotaParaPrimoMalo :: [(Int, Int)] -> Int
cotaParaPrimoMalo sist = cotaParaPrimoMaloDesde sist 1

--  [22, 25] 9 --> 0. El 23 (acotado) no divide ni a 22 ni a 25.
cantidadMultiplos :: [Int] -> Int -> Int
cantidadMultiplos [] _ = 0
cantidadMultiplos (m:ms) n | mod m (nEsimoPrimo n) == 0 = 1 + cantidadMultiplos ms n
                           | otherwise = cantidadMultiplos ms n

-- Si n divide a, por lo menos, 2 módulos del sistema, entonces es "primo malo". (True)
-- [22,25] --> False, luego 23 no es primo malo. Habrá solución final para el sistema.
esPrimoMalo :: [(Int, Int)] -> Int -> Bool
esPrimoMalo sist n = cantidadMultiplos (modulos sist) n >= 2

-- Dados un sistema, devuelve una lista con todos los primos malos del mismo.
todosLosPrimosMalosHasta :: [(Int, Int)] -> Int -> [Int]
todosLosPrimosMalosHasta _ 0 = []
todosLosPrimosMalosHasta sist n | esPrimoMalo sist n = (nEsimoPrimo n):(todosLosPrimosMalosHasta sist (n-1))
                                | otherwise = todosLosPrimosMalosHasta sist (n-1)

-- Dado un sistema, y el n-esimo primo (<= módulo más alto), devuelve una lista con todos los primos malos
-- [22,25] --> []
todosLosPrimosMalos :: [(Int, Int)] -> [Int]
todosLosPrimosMalos [] = []
todosLosPrimosMalos sist = todosLosPrimosMalosHasta sist (cotaParaPrimoMalo sist)


-- 4 --------------------
-- El codigo a continuacion parte de la premisa de que todos los módulos son potencia de un mismo primo.
-- Me dara como resultado una UNICA ECUACION que resumirá al resto de las mismas.

-- Dadas dos ecuaciones (simplificadas y ordenadas) decide si descartar ambas o quedarse con la de mayor mód.
solucDosEcPotenciasPrimoOrd :: (Int, Int) -> (Int, Int) -> (Int, Int)
solucDosEcPotenciasPrimoOrd (r1, m1) (r2, m2) | mod (r2-r1) m1 == 0 = (r2, m2)
                                              | otherwise = undefined

-- Ordena dos ecuaciones, es decir, coloca la de menor módulo primera.
solucDosEcPotenciasPrimo :: (Int, Int) -> (Int, Int) -> (Int, Int)
solucDosEcPotenciasPrimo (r1, m1) (r2, m2) | m1 <= m2 = solucDosEcPotenciasPrimoOrd (r1, m1) (r2, m2)
                                           | otherwise = solucDosEcPotenciasPrimoOrd (r2, m2) (r1, m1)

-- Resuelve un sistema en el que todos los módulos son potencias de un mismo primo.
solucSistemaPotenciasPrimo :: [(Int, Int)] -> (Int, Int)
solucSistemaPotenciasPrimo [e] = e
solucSistemaPotenciasPrimo (e1:e2:es) = solucSistemaPotenciasPrimo ((solucDosEcPotenciasPrimo e1 e2):es)


-- 5 --------------------

-- Dado un sistema y un primo p (MALO), devuelve los dos sistemas que surgen al desdoblar cada ecuación del sistema original
-- según el primo p.
desdoblarSistemaEnFcionPrimo :: [(Int, Int)] -> Int -> ([(Int, Int)], [(Int, Int)])
desdoblarSistemaEnFcionPrimo [] _ = ([], [])
desdoblarSistemaEnFcionPrimo ((r, m):es) p -- si el primo malo p no divide a m, la ecuacion "va a la segunda parte".
                                           | k == 0 = (pri, (r, m):seg)
                                           -- si m es una potencia de p, la ecuacion "va a la primera parte".
                                           | m == p^k = ((r, m):pri, seg)
                                           -- si m es multiplo de p sin ser potencia de p, se agrega una ecuacion a la primera parte, y otra a la segunda.
                                           | otherwise = ((mod r (p^k), p^k):pri, (mod r (div m (p^k)), div m (p^k)):seg)
 where -- funcion recursiva que desdobla la cola del sistema de ecuaciones.
       (pri, seg) = desdoblarSistemaEnFcionPrimo es p
       -- k es la mayor potencia de p que divide a m
       k = quePotenciaLoDivide m p

-- la siguiente funcion toma la funcion solucSistemaPotenciasPrimo que resuelve un sistema donde los módulos son potencia de un mismo primo
--
sistemaEquivSinPrimosMalosAux :: [(Int, Int)] -> [Int] -> [(Int, Int)]
sistemaEquivSinPrimosMalosAux sist [] = sist
-- caso interesante: si todavia hay un primo malo en el sistema,
sistemaEquivSinPrimosMalosAux sist (p:ps) = (solucSistemaPotenciasPrimo pri):(sistemaEquivSinPrimosMalosAux seg ps)
 where (pri, seg) = desdoblarSistemaEnFcionPrimo sist p

-- Dado un sistema, devuelve un sistema equivalente sin primos malos.
sistemaEquivSinPrimosMalos :: [(Int, Int)] -> [(Int, Int)]
sistemaEquivSinPrimosMalos sist = sistemaEquivSinPrimosMalosAux sist (todosLosPrimosMalos sist)


-- 6 -------------------

solucSistemaModCoprimos :: [(Int, Int)] -> (Int, Int)
solucSistemaModCoprimos [e] = e
solucSistemaModCoprimos ((r1, m1):(r2, m2):es) = solucSistemaModCoprimos ((r, m1*m2):es)
 where (d, s, t) = emcd m1 m2
       r = mod (r1*t*m2 + r2*s*m1) (m1*m2)

-- Dado un sistema, lo simplifica,
-- lo reemplaza por un sist equivalente sin primos malos ( que dividen al menos a dos módulos de m1, m2, ..., ml)
-- y, finalmente, va descartando las ecuaciones restantes hasta llegar a la solución final.
solucSistema :: [(Int, Int, Int)] -> (Int, Int)
solucSistema sist = solucSistemaModCoprimos ( sistemaEquivSinPrimosMalos ( sistemaSimplifEquiv sist) )

-- Ej 1: dado un sistema general, decide si cada una de sus ecuaciones vista independientemente de las otras, tiene solucion.
cadaEcTieneSoluc :: [(Int, Int, Int)] -> Bool
cadaEcTieneSoluc [e] = True
cadaEcTieneSoluc ((a, b, m):es) = b `mod` d == 0 && cadaEcTieneSoluc es
      where d = mcd a m

-- Ej 2:  dado un sistema simplificado, decide si tiene solucion.
solucDosEcPotenciasPrimoOrd' :: (Int, Int) -> (Int, Int) -> (Int, Int)
solucDosEcPotenciasPrimoOrd' (r1, m1) (r2,m2) | mod (r2 - r1) m1 == 0 = (r2, m2)
                                              | otherwise = (0,0)

solucDosEcPotenciasPrimo' :: (Int, Int) -> (Int, Int) -> (Int, Int)
solucDosEcPotenciasPrimo' (r1, m1) (r2, m2) | m1 <= m2 = solucDosEcPotenciasPrimoOrd' (r1, m1) (r2, m2)
                                            | otherwise = solucDosEcPotenciasPrimo' (r2, m2) (r1, m1)

solucSistemaPotenciasPrimo' :: [(Int, Int)] -> Bool
solucSistemaPotenciasPrimo' [e] = True
solucSistemaPotenciasPrimo' (e1:e2:es)| (solucDosEcPotenciasPrimo' e1 e2) /= (0,0) = solucSistemaPotenciasPrimo'((solucDosEcPotenciasPrimo' e1 e2): es)
                                      | solucDosEcPotenciasPrimo' e1 e2 == (0,0) = False

sistemaEquivSinPrimosMalosAux' :: [(Int, Int)] -> [Int] -> Bool
sistemaEquivSinPrimosMalosAux' sist [] = True
sistemaEquivSinPrimosMalosAux' sist (p:ps) | (solucSistemaPotenciasPrimo' pri) = (sistemaEquivSinPrimosMalosAux' seg ps)
                                         | otherwise = False
                                           where
                                             (pri,seg) = desdoblarSistemaEnFcionPrimo sist p

sistemaEquivSinPrimosMalos' :: [(Int, Int)] -> Bool
sistemaEquivSinPrimosMalos' sist = sistemaEquivSinPrimosMalosAux' sist (todosLosPrimosMalos sist)

tieneSolucionSimplif :: [(Int, Int)] -> Bool
tieneSolucionSimplif sist = sistemaEquivSinPrimosMalos' sist
