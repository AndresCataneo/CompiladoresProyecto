module AFD (AFD(..), acepta, afnToAfd) where

-- Importamos 'transicionesAfn' que es el nombre correcto del campo en AFN.hs
import AFN (AFN(..), transicionesAfn)
import Data.List (sort, nub, find)
import Data.Maybe (fromJust) 

-- Definicion de las transiciones (Estado, caracter, Estado)
type Transicion = (Int, Char, Int)

-- Definicion del AFD
data AFD = AFD
    {
        estadosAfd :: [Int],
        alfabetoAfd :: [Char],
        transicionesAfd :: [Transicion],
        inicialAfd :: Int,
        finalesAfd :: [Int]
    } deriving (Show)

-- Revisa si la cadena dada es aceptada por el AFD.
acepta :: AFD -> String -> Bool
acepta afd s =
    let 
        estadoFinal = transita (transicionesAfd afd) (inicialAfd afd) s
    in 
        -- Comprueba si el estado al que llegamos es un estado final.
        estadoFinal `elem` finalesAfd afd


{-
Funcion que recive el conjunto de transiciones, un estado y una cadena.
Procesa la cadena carácter por carácter y devuelve el estado en el
que termina el autómata después de leer toda la cadena.
-}
transita :: [Transicion] -> Int -> String -> Int
transita _ estadoActual [] = estadoActual -- Caso base: cadena vacia
transita transiciones estadoActual (c:cs) =
    case siguienteEstado transiciones estadoActual c of
        Just proximoEstado -> transita transiciones proximoEstado cs
        Nothing -> -1 -- Estado  sink state

{-
Funcion que nos devuelve el siguiente estado del AFD de acuerdo con las transiciones.
Usa una función auxiliar 'coincide' en lugar de una lambda.
-}
siguienteEstado :: [Transicion] -> Int -> Char -> Maybe Int
siguienteEstado transiciones estadoOrigen char =
    -- 'find' ahora usa la función auxiliar 'coincide'
    case find coincide transiciones of
        Just (_, _, destino) -> Just destino
        Nothing -> Nothing
    where
        -- Función auxiliar para la búsqueda:
        -- Devuelve True si una transición coincide con el origen y el char.
        coincide :: Transicion -> Bool
        coincide (origen, simbolo, _) =
            origen == estadoOrigen && simbolo == char

-- Convierte un AFN a AFD usando listas de subconjuntos (construcción de subconjuntos).
afnToAfd :: AFN -> AFD
afnToAfd afn =
    let
        -- El estado inicial del AFD es el conjunto que contiene el estado inicial del AFN. 
        q0 = sort [inicialAfn afn]

        -- Inicia la construcción recursiva.
        (finalStates, finalTransitions) = buildDfa afn [q0] [(q0, 0)] []

        -- Un estado del AFD es final si su conjunto de estados 
        -- contiene al menos un estado final del AFN original.
        
        finales = 
            [ dfaId 
            | (nfaStates, dfaId) <- finalStates, 
            any (`elem` finalesAfn afn) nfaStates ]
    in
        AFD
        {
            estadosAfd      = sort $ map snd finalStates,
            alfabetoAfd     = alfabetoAfn afn,
            transicionesAfd = finalTransitions,
            inicialAfd      = 0, -- El estado inicial siempre es 0
            finalesAfd      = sort finales
        }

{-
Función recursiva para la construcción de subconjuntos.
Usa una "Worklist" para rastrear los estados del AFD que necesitan ser procesados.
-}
buildDfa :: AFN -> [[Int]] -> [([Int], Int)] -> [Transicion] -> ([([Int], Int)], [Transicion])
buildDfa _ [] knownStates transitions = (knownStates, transitions) -- Caso base: worklist vacia
buildDfa afn (currentSet:worklist) knownStates transitions =
    let
        -- Obtenemos el ID entero para el conjunto actual de estados AFN.
        currentId = fromJust $ lookup currentSet knownStates

        {-
        FUNCIÓN AUXILIAR
        
        Definida DENTRO del 'let' para que pueda "capturar" las variables
        'afn', 'currentSet' y 'currentId' de la función 'buildDfa'.
        
        Procesa un solo caracter del alfabeto y actualiza los acumuladores.
        -}
        procesarCaracter :: Char -> ([Transicion], [([Int], Int)], [[Int]]) -> ([Transicion], [([Int], Int)], [[Int]])
        procesarCaracter char (accTrans, accStates, accWorklist) =
            let
                -- 'mover' calcula el conjunto de estados alcanzables
                nextSet = mover afn currentSet char
            in
                -- Si el conjunto está vacío, no hay transición.
                if null nextSet
                then (accTrans, accStates, accWorklist)
                else
                    -- Revisa si ya hemos visto este conjunto de estados.
                    case lookup nextSet accStates of
                        -- CASO 1: Ya lo hemos visto, solo agregamos la transición.
                        Just nextId -> 
                            let newT = (currentId, char, nextId)
                            in (newT : accTrans, accStates, accWorklist)
                        
                        -- CASO 2: Es un estado nuevo del AFD.
                        Nothing ->
                            let
                                newId = length accStates -- Asignamos un nuevo ID
                                newT = (currentId, char, newId)
                                updatedStates = (nextSet, newId) : accStates
                            in
                                -- Agregamos la nueva transición,
                                -- el nuevo estado al mapa de estados,
                                -- y el nuevo estado a la worklist.
                                (newT : accTrans, updatedStates, nextSet : accWorklist)
        
        -- Procesamos todos los caracteres del alfabeto.
        (newTransitions, newStates, newWorklistItems) = foldr
            procesarCaracter      -- <-- Función auxiliar
            ([], knownStates, []) -- Acumulador inicial
            (alfabetoAfn afn)     -- Lista a procesar

    in
        -- Llamada recursiva con los nuevos estados y transiciones encontrados.
        buildDfa afn (worklist ++ newWorklistItems) newStates (transitions ++ newTransitions)

{-
Calcula el conjunto de estados alcanzables desde una lista de estados en un carácter.
Devuelve una lista ordenada sin duplicados.
-}
mover :: AFN -> [Int] -> Char -> [Int]
mover afn states char =
    let
        destinations = 
            [ nextState
            | currentState <- states
            , (origin, symbol, dests) <- transicionesAfn afn 
            , currentState == origin && symbol == char
            , nextState <- dests ]
    in
        -- 'nub' elimina duplicados
        nub (sort destinations)