module AFN (AFN(..), eliminarEpsilonTrans, epsilonClosure) where

import AFNep
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

{-
Definición del tipo AFN sin transiciones epsilon
-}

type Transiciones = (Int, Char, [Int])

data AFN = AFN
    {
        estadosAfn :: [Int],
        alfabetoAfn :: [Char],
        transicionesAfn :: [Transiciones],
        inicialAfn :: Int, 
        finalesAfn :: [Int]
    } deriving (Show)


{-
Funcion epsilonClosure
Devuelve todos los estados alcanzables desde un conjunto de estados
usando cero o más transiciones epsilon
-}

epsilonClosure :: AFNEp -> [Int] -> [Int]
epsilonClosure afn conjEstados = Set.toList (close Set.empty conjEstados)
    where 
        close visitados [] = visitados
        close visitados (x:xs)
            | x `Set.member` visitados = close visitados xs --Si x ya fue visitado pasa a xs
            | otherwise =
                let dstConEpsilon = [estDestino
                                    | (estOrigen, Nothing, estsDestino) <- transiciones afn, --recorre los estados y agarra las que son epsilon
                                    estOrigen == x,
                                    estDestino <- estsDestino
                                    ]
                in close (Set.insert x visitados) ( dstConEpsilon ++ xs)

{-
Función para construir el AFN(Sin transiciones epsilon)
Reconstruye las transiciones del nuevo AFN considerando estas cerraduras,
para cada estado y símbolo del alfabeto, se determinan los posibles destinos 
alcanzables aplicando primero la cerradura, luego las transiciones normales, 
y finalmente otra closure sobre los estados alcanzados.
-}

construirAFN :: AFNEp -> [Transiciones] -> [Int] -> AFN
construirAFN afnEp trans finales' = AFN
    {estadosAfn = estados afnEp,
     alfabetoAfn = alfabeto afnEp,
     transicionesAfn = trans,
     inicialAfn = inicial afnEp,
     finalesAfn = finales'
     }


{-
función quetoma un autómata finito no determinista con transiciones épsilon (AFNEp)
y devuelve un nuevo autómata sin transiciones épsilon (AFN).
-}

eliminarEpsilonTrans :: AFNEp -> AFN
eliminarEpsilonTrans afnEp = construirAFN afnEp nuevaTrans nuevosFinales
    where
        closures = [(q, epsilonClosure afnEp [q]) | q <- estados afnEp] --Lista de todas las ε-closures calculados una vez

        -- Función auxiliar que usa lookup
        lookupCierre q = fromMaybe [] (lookup q closures) -- Busca un estado en la cerradura y valor por defecto si no encuentra el estado

        -- Generar nuevas transiciones sin epsilon
        nuevaTrans = 
            [(q, c, destinos q c)
            | q <- estados afnEp,
              c <- alfabeto afnEp,
              not (null (destinos q c))]

        destinos q c =
            let cierreQ = lookupCierre q -- obtenemos ε-closure(q)
                movs = [ dst
                       | p <- cierreQ,
                         (p', Just x, dsts) <- transiciones afnEp,
                         p == p', x == c,
                         dst <- dsts]

            in Set.toList . Set.fromList . concatMap (epsilonClosure afnEp . pure) $ movs

         -- Un estado será final si su cierre contiene algún estado final del original
        nuevosFinales = 
            [q
            |(q, cierreQ) <- closures,
             any (`elem` final afnEp) cierreQ]