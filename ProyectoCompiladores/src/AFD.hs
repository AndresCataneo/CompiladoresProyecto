module AFD (AFD(..), afnToAfd, construirAFD, mover, Transicion) where
import AFN (AFN(..))
import Data.List (sort, nub)
import Data.Maybe (fromMaybe)

{--
Recibe un estado origen, un símbolo del alfabeto y un estado destino
--}
type Transicion = (Int, Char, Int)

{--
Constructor del AFD
--}
data AFD = AFD
    { estadosAfd :: [Int]
    , alfabetoAfd :: [Char]
    , transicionesAfd :: [Transicion]
    , inicialAfd :: Int
    , finalesAfd :: [Int]}
    
    
instance Show AFD where
    show afd =
        "========== AFD ==========\n"
        ++ "Estados: " ++ show (estadosAfd afd) ++ "\n"
        ++ "Alfabeto: " ++ show (alfabetoAfd afd) ++ "\n"
        ++ "Transiciones:\n"
        ++ unlines [ "  δ(" ++ show e1 ++ ", '" ++ [s] ++ "') = " ++ show e2
                   | (e1, s, e2) <- transicionesAfd afd ]
        ++ "Estado inicial: " ++ show (inicialAfd afd) ++ "\n"
        ++ "Estados finales: " ++ show (finalesAfd afd) ++ "\n"
        ++ "========================="

{--
Funcion principal que convierte un AFN a un AFD usando el conjunto potencia de los estados del AFN

--}
afnToAfd :: AFN -> AFD
afnToAfd afn =
    let
        q0 = sort [inicialAfn afn]
        --(qn,n) para poder identificar los conjuntos de estados del AFN y asignarles un id único en el AFD
        (edoFinales, transFinales) = construirAFD afn [q0] [(q0, 0)] []

        --Los estados finales del AFD son aquellos conjuntos que contienen al menos un estado final del AFN
        finales =[ afdId | (afnEstados, afdId) <- edoFinales, any (`elem` finalesAfn afn) afnEstados]
    in
        AFD
        { estadosAfd      = sort $ map snd edoFinales, --Tomamos los ids de los conjuntos de estados
        alfabetoAfd     = alfabetoAfn afn,
        transicionesAfd = transFinales,
        inicialAfd      = 0,
        finalesAfd      = sort finales
        }

{--
Entrada:
    - AFN original
    - Lista de conjuntos de estados por procesar
    - Lista de conjuntos de estados ya conocidos con su id asignado
    - Lista de transiciones del AFD construidas hasta el momento
Salida:
    - Lista de conjuntos de estados conocidos con su id asignado
    - Lista de transiciones del AFD construidas
--}
construirAFD :: AFN -> [[Int]] -> [([Int], Int)] -> [Transicion] -> ([([Int], Int)], [Transicion])
-- Caso base
    -- Si no hay más conjuntos por procesar, devolvemos los conocidos y las transiciones construidas hasta el momento
construirAFD _ [] edosConocidos transiciones = (edosConocidos, transiciones)
-- Caso de recursion
construirAFD afn (cjtoActual:cjtosPorProcesar) edosConocidos transiciones =
    let
        --Buscamos el id del conjunto actual en los estados conocidos, si no lo encontramos devolvemos -1
            --Por como lo construi, no deberia pasar, pero por si acaso lo pongo, aunque con formJust deberia ser suficiente
        idActual = fromMaybe (-1) $ lookup cjtoActual edosConocidos
        alfabeto = alfabetoAfn afn
        -- Calculamos el conjunto siguiente de estados desde el conjunto actual para cada símbolo del alfabeto
        (nuevasTrans, edosActualizados, nuevosEdosEncontrados) = foldl
            (\(transAcumuladas, edosAcumulados, nuevosEdosEnc) c ->
                --Estados alcanzables desde el conjunto actual con el símbolo c
                let sigConjunto = sort (mover afn cjtoActual c)
                in if null sigConjunto
                    then (transAcumuladas, edosAcumulados, nuevosEdosEnc)
                    else
                        --En caso de que el conjunto siguiente ya haya sido encontrado, reutilizamos su id al agregar la transición
                        case lookup sigConjunto edosAcumulados of
                            Just sigId ->
                                ((idActual, c, sigId):transAcumuladas, edosAcumulados, nuevosEdosEnc)
                            Nothing ->
                                let nuevoId = length edosAcumulados
                                in ((idActual, c, nuevoId):transAcumuladas,
                                    (sigConjunto, nuevoId):edosAcumulados,
                                    nuevosEdosEnc ++ [sigConjunto])
            )
            ([], edosConocidos, [])
            alfabeto
    in
        construirAFD afn (cjtosPorProcesar ++ nuevosEdosEncontrados) edosActualizados (transiciones ++ nuevasTrans)

{--
Funcion que devuelve los estados alcanzables desde un conjunto de estados 
con el simbolo dado en una transición
--}
mover :: AFN -> [Int] -> Char -> [Int]
mover afn estados simbolo =
    let
        --Agregamos a a la lista si el estado origen y el símbolo coinciden
        destinos = [ edoDest | edoActual <- estados
                    , (edoOrig, simb, edosAct) <- transicionesAfn afn
                    , edoOrig == edoActual && simb == simbolo
                    , edoDest <- edosAct
                    ]
    in nub (sort destinos)
