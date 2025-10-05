module AFNep (AFNEp(..), reToAFNEp, sigma) where

import RE 
import Data.Set (toList, fromList)

--Funcion que obtiene el alfabeto de una expresion regular. Esto es para definir el alfabeto de nuestro lenguaje en el AFN-ε
sigma :: RE -> [Char]
sigma Vacio = []
sigma Epsilon = []
sigma (Symbol c) = [c]
sigma (Concatena e1 e2) = toList $ fromList (sigma e1 ++ sigma e2)
sigma (Union e1 e2) = toList $ fromList (sigma e1 ++ sigma e2)
sigma (Estrella e) = sigma e

--Transicion epsilon
type TransEp = (Int, Maybe Char, [Int])

--Estado
type Estado = Int

--AFN con transiciones epsilon
data AFNEp = AFNEp {estados :: [Estado], 
        alfabeto :: [Char], 
        transiciones :: [TransEp], 
        inicial :: Estado, 
        final :: [Estado]}
        deriving (Show)

--Traducción de expresiones regulares a AFN con transiciones epsilon
reToAFNEp :: RE -> AFNEp
reToAFNEp e = reToAFNEpAux e 0

reToAFNEpAux :: RE -> Estado -> AFNEp
-- Caso vacío
reToAFNEpAux Vacio n = AFNEp {estados = [n, n+1],
                                alfabeto = [],
                                transiciones = [],
                                inicial = n,
                                final = [n+1]}
-- Caso Epsilon
reToAFNEpAux Epsilon n = AFNEp {estados = [n, n+1],
                                alfabeto = [],
                                transiciones = [(n, Nothing, [n+1])],
                                inicial = n,
                                final = [n+1]}
--Caso base, un símbolo
reToAFNEpAux (Symbol c) n = AFNEp {estados = [n, n+1],
                                alfabeto = [c],
                                transiciones = [(n, Just c, [n+1])],
                                inicial = n,
                                final = [n+1]}
--Caso union
reToAFNEpAux (Union m1 m2) n = AFNEp {estados = [n] ++ estados afn1 ++ estados afn2,
                                alfabeto = toList $ fromList (alfabeto afn1 ++ alfabeto afn2),
                                transiciones = (transiciones afn1) ++ (transiciones afn2) ++
                                                [ (n, Nothing, [inicial afn1, inicial afn2])],
                                inicial = n,
                                final = final afn1 ++ final afn2 }
        where 
            afn1 = reToAFNEpAux m1 (n+1)
            edoIniM2 = maximum (estados afn1) + 1 
            afn2 = reToAFNEpAux m2 edoIniM2
--Caso concatenacion
reToAFNEpAux (Concatena m1 m2) n = AFNEp {estados = estados afn1 ++ estados afn2,
                                alfabeto = toList $ fromList (alfabeto afn1 ++ alfabeto afn2),
                                transiciones = (transiciones afn1) ++ (transiciones afn2) ++
                                                [(f, Nothing, [inicial afn2]) | f <- final afn1],
                                inicial = inicial afn1,
                                final = final afn2 }
        where               
            afn1 = reToAFNEpAux m1 n
            edoIniM2 = maximum (estados afn1) + 1 
            afn2 = reToAFNEpAux m2 edoIniM2
--Caso estrella
reToAFNEpAux (Estrella m) n = AFNEp {estados = estados afn1,
                                alfabeto = alfabeto afn1,
                                transiciones = (transiciones afn1) ++ [(f, Nothing, [inicial afn1]) | f <- final afn1],
                                inicial = inicial afn1,
                                final = [inicial afn1] ++ final afn1}
        where
            afn1 = reToAFNEpAux m n