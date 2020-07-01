module CodificacionDeListas
where
import Clase05

{--
longitud n, dado un numero natural n calcula la longitud de la lista que codifica
longitud 132 = 5
--}
longitud :: Int -> Int
longitud n = longitudDesde n 1

longitudDesde :: Int -> Int -> Int
longitudDesde 1 _ = 0
longitudDesde n k = 1 + (longitudDesde (n `div` (p^a)) (k + 1))
        where p = nEsimoPrimo k
              a = quePotenciaLoDivide n p

quePotenciaLoDivide :: Int -> Int -> Int
quePotenciaLoDivide n p | n `mod` p == 0 = 1 + quePotenciaLoDivide (n `div` p) p
                        | otherwise = 0

{--
iesimo n i, dados dos números naturales n e i, devuelve el iésimo elemento de la lista que codifica n.
iésimo 132 2 = 1
--}
iesimo :: Int -> Int -> Int
iesimo n i = quePotenciaLoDivide n (nEsimoPrimo i)
