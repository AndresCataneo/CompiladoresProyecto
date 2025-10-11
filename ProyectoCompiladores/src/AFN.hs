module AFN (AFN(..), eliminarEpsilon, epsilonclosure) where

import AFNep
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

{-
Definición del tipo AFN sin transiciones epsilon
-}

type Transicion = (Int, Char, [Int])

data AFN = AFN
    {
        estadoAfn :: [Int],
        alfabetoAfn :: [Char],
        transiciones :: [Transicion],
        inicialAfn :: Int, 
        finalesAfn :: [Int]
    } deriving (Show)


{-
Funcion epsilonClosure
Devuelve todos los estados alcanzables desde un conjunto de estados
usando cero o más transiciones epsilon
-}

epsilonClosure :: AFNEp -> [Int] -> [Int]
epsilonClosure afn estados = Set.toList (cerrar Set.empty estados)
    where 
        close visitados [] = visitados
        close visitados (x:xs)
            | x `Set.member` visitados = close visitados xs --Si x ya fue visitado pasa a xs
            | otherwise
                let dstConEpsilon = [estDestino
                                | (estOrigen, Nothing, estsDestino) <- transiciones afn, --recorre los estados y agarra las que son epsilon
                                estOrigen == x,
                                estDestino <- estsDestino
                                ]
                in close (Set.insert x visitados) ( dstConEpsilon ++ xs)