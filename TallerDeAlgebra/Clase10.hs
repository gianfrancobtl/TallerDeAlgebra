module Clase10
where
import Clase05
import codificacionDeListas
import Clase09


-- 1 ---------------------

-- Toma la ecuacion dada y devuelve otra donde a y m son coprimos.
ecEquivalente :: (Int, Int, Int) -> (Int, Int, Int)
ecEquivalente (a, b, m) | mod b d /= 0  = undefined
                        | otherwise = (div a d, div b d, div m d)
 where d = mcd a m

solucionEcConPropAdic :: (Int, Int, Int) -> (Int, Int)
solucionEcConPropAdic (a, b, m) = (mod (s*b) m, m)
 where (d, s, t) = emcd a m

solucionEc :: (Int, Int, Int) -> (Int, Int)
solucionEc e = solucionEcConPropAdic (ecEquivalente e)


-- 2 --------------------
--
-- sistemaSimplifEquiv :: [(Int, Int, Int)] -> [(Int, Int)]
-- sistemaSimplifEquiv [] = []
-- sistemaSimplifEquiv (e:es) = (solucionEc e):(sistemaSimplifEquiv es)
--
--
-- -- 3 --------------------
--
-- modulos :: [(Int, Int)] -> [Int]
-- modulos [] = []
-- modulos ((r, m):es) = m:(modulos es)
--
-- mayorModulo :: [(Int, Int)] -> Int
-- mayorModulo sist = maximum (modulos sist)
--
-- cotaParaPrimoMaloDesde :: [(Int, Int)] -> Int -> Int
-- cotaParaPrimoMaloDesde sist n | nEsimoPrimo (n+1) > (mayorModulo sist) = n
--                               | otherwise = cotaParaPrimoMaloDesde sist (n+1)
--
-- cotaParaPrimoMalo :: [(Int, Int)] -> Int
-- cotaParaPrimoMalo sist = cotaParaPrimoMaloDesde sist 1
--
-- cantidadMultiplos :: [Int] -> Int -> Int
-- cantidadMultiplos [] _ = 0
-- cantidadMultiplos (m:ms) n | mod m (nEsimoPrimo n) == 0 = 1 + cantidadMultiplos ms n
--                            | otherwise = cantidadMultiplos ms n
--
-- esPrimoMalo :: [(Int, Int)] -> Int -> Bool
-- esPrimoMalo sist n = cantidadMultiplos (modulos sist) n >= 2
--
--
-- todosLosPrimosMalosHasta :: [(Int, Int)] -> Int -> [Int]
-- todosLosPrimosMalosHasta _ 0 = []
-- todosLosPrimosMalosHasta sist n | esPrimoMalo sist n = (nEsimoPrimo n):(todosLosPrimosMalosHasta sist (n-1))
--                                 | otherwise = todosLosPrimosMalosHasta sist (n-1)
--
-- todosLosPrimosMalos :: [(Int, Int)] -> [Int]
-- todosLosPrimosMalos [] = []
-- todosLosPrimosMalos sist = todosLosPrimosMalosHasta sist (cotaParaPrimoMalo sist)
--
--
-- -- 4 --------------------
--
-- solucDosEcPotenciasPrimoOrd :: (Int, Int) -> (Int, Int) -> (Int, Int)
-- solucDosEcPotenciasPrimoOrd (r1, m1) (r2, m2) | mod (r2-r1) m1 == 0 = (r2, m2)
--                                               | otherwise = undefined
--
-- solucDosEcPotenciasPrimo :: (Int, Int) -> (Int, Int) -> (Int, Int)
-- solucDosEcPotenciasPrimo (r1, m1) (r2, m2) | m1 <= m2 = solucDosEcPotenciasPrimoOrd (r1, m1) (r2, m2)
--                                            | otherwise = solucDosEcPotenciasPrimoOrd (r2, m2) (r1, m1)
--
-- solucSistemaPotenciasPrimo :: [(Int, Int)] -> (Int, Int)
-- solucSistemaPotenciasPrimo [e] = e
-- solucSistemaPotenciasPrimo (e1:e2:es) = solucSistemaPotenciasPrimo ((solucDosEcPotenciasPrimo e1 e2):es)
--
--
-- -- 5 --------------------
--
-- desdoblarSistemaEnFcionPrimo :: [(Int, Int)] -> Int -> ([(Int, Int)], [(Int, Int)])
-- desdoblarSistemaEnFcionPrimo [] _ = ([], [])
-- desdoblarSistemaEnFcionPrimo ((r, m):es) p | k == 0 = (pri, (r, m):seg)
--                                            | m == p^k = ((r, m):pri, seg)
--                                            | otherwise = ((mod r (p^k), p^k):pri, (mod r (div m (p^k)), div m (p^k)):seg)
--  where (pri, seg) = desdoblarSistemaEnFcionPrimo es p
--        k = quePotenciaLoDivide m p
--
-- sistemaEquivSinPrimosMalosAux :: [(Int, Int)] -> [Int] -> [(Int, Int)]
-- sistemaEquivSinPrimosMalosAux sist [] = sist
-- sistemaEquivSinPrimosMalosAux sist (p:ps) = (solucSistemaPotenciasPrimo pri):(sistemaEquivSinPrimosMalosAux seg ps)
--  where (pri, seg) = desdoblarSistemaEnFcionPrimo sist p
--
-- sistemaEquivSinPrimosMalos :: [(Int, Int)] -> [(Int, Int)]
-- sistemaEquivSinPrimosMalos sist = sistemaEquivSinPrimosMalosAux sist (todosLosPrimosMalos sist)
--
--
-- -- 6 -------------------
--
-- solucSistemaModCoprimos :: [(Int, Int)] -> (Int, Int)
-- solucSistemaModCoprimos [e] = e
-- solucSistemaModCoprimos ((r1, m1):(r2, m2):es) = solucSistemaModCoprimos ((r, m1*m2):es)
--  where (d, s, t) = emcd m1 m2
--        r = mod (r1*t*m2 + r2*s*m1) (m1*m2)
--
-- solucSistema :: [(Int, Int, Int)] -> (Int, Int)
-- solucSistema sist = solucSistemaModCoprimos ( sistemaEquivSinPrimosMalos ( sistemaSimplifEquiv sist) )
