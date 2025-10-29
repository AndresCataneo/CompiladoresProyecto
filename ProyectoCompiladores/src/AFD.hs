module AFD (AFD(..), afnToAfd, construirAFD, mover) where
import AFN (AFN(..))
import Data.List (sort, nub)
import Data.Maybe (fromJust)


type Transicion = (Int, Char, Int)

data AFD = AFD
    { estadosAfd :: [Int]
    , alfabetoAfd :: [Char]
    , transicionesAfd :: [Transicion]
    , inicialAfd :: Int
    , finalesAfd :: [Int]
    } deriving (Show)

afnToAfd :: AFN -> AFD
afnToAfd afn =
    let
        q0 = sort [inicialAfn afn]
        (edoFinales, transFinales) = construirAFD afn [q0] [(q0, 0)] []

        finales =
            [ dfaId
            | (afnEstados, dfaId) <- edoFinales
            , any (`elem` finalesAfn afn) afnEstados
            ]
    in
        AFD
        { estadosAfd      = sort $ map snd edoFinales
        , alfabetoAfd     = alfabetoAfn afn
        , transicionesAfd = transFinales
        , inicialAfd      = 0
        , finalesAfd      = sort finales
        }

construirAFD :: AFN -> [[Int]] -> [([Int], Int)] -> [Transicion] -> ([([Int], Int)], [Transicion])
construirAFD _ [] edosConocidos transiciones = (edosConocidos, transiciones)
construirAFD afn (cjtoActual:cjtosPorProcesar) edosConocidos transiciones =
    let
        currentId = fromJust $ lookup cjtoActual edosConocidos
        alf = alfabetoAfn afn

        (nuevasTrans, edosActualizados, nuevosEdosEncontrados) = foldl
            (\(tAcc, sAcc, wAcc) c ->
                let nextSet = sort (mover afn cjtoActual c)
                in if null nextSet
                    then (tAcc, sAcc, wAcc)
                    else
                        case lookup nextSet sAcc of
                            Just sigId ->
                                ((currentId, c, sigId):tAcc, sAcc, wAcc)
                            Nothing ->
                                let nuevoId = length sAcc
                                in ((currentId, c, nuevoId):tAcc,
                                    (nextSet, nuevoId):sAcc,
                                    wAcc ++ [nextSet])
            )
            ([], edosConocidos, [])
            alf
    in
        construirAFD afn (cjtosPorProcesar ++ nuevosEdosEncontrados) edosActualizados (transiciones ++ nuevasTrans)

mover :: AFN -> [Int] -> Char -> [Int]
mover afn states char =
    let
        destinos = [ d
                    | s <- states
                    , (o, c, ds) <- transicionesAfn afn
                    , o == s && c == char
                    , d <- ds
                    ]
    in nub (sort destinos)
--Prueba (0+1)*01
--AFN
{--afn1 = AFN {
    estadosAfn = [0,1,2],
    alfabetoAfn = ['0','1'],
    transicionesAfn = [
        (0,'0',[0]),
        (0,'1',[0]),
        (0,'0',[1]),
        (1,'1',[2])
    ],
    inicialAfn = 0,
    finalesAfn = [2]
}
--}
--AFD
--afd1 = afnToAfd afn1

--Prueba2 (0+1)*1(((0+1)(0+1))+0)
--AFN

afn2 = AFN {
    estadosAfn = [0,1,2,3],
    alfabetoAfn = ['0','1'],
    transicionesAfn = [
        (0,'0',[0]),
        (0,'1',[0]),
        (0,'1',[1]),
        (1,'0',[2]),
        (1,'1',[2]),
        (1,'0',[3]),
        (2,'0',[3]),
        (2,'1',[3])
    ],
    inicialAfn = 0,
    finalesAfn = [3]
}


--AFD
afd2 = afnToAfd afn2
