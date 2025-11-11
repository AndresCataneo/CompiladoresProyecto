-- Declaración del módulo MDD
module MDD (MDD(..), construyeMDD, siguienteEstadoMDD) where

-- Importamos modulos necesarios
import AFD(AFD(..)) 

-- Definimos sinónimos de tipos para mayor claridad
type Etiqueta = String
type Estado = Int
type Transicion = (Estado, Char, Estado)

-- Definimos el tipo de dato algebraico MDD
data MDD = MDD
    {
        estadosMDD :: [Estado],
        alfabetoMDD :: [Char],
        transicionesMDD :: [Transicion],
        inicialMDD :: Estado,
        finalesMDD :: [Estado],
        finalesConEtiquetasMDD :: [(Estado, Etiqueta)]  
    } 

-- Implementamos la instancia Show para el tipo MDD
instance Show MDD where
    show mdd =
        "========== MDD ==========\n"
        ++ "Estados: " ++ show (estadosMDD mdd) ++ "\n"
        ++ "Alfabeto: " ++ show (alfabetoMDD mdd) ++ "\n"
        ++ "Transiciones:\n"
        ++ unlines [ "  δ(" ++ show e1 ++ ", '" ++ [s] ++ "') = " ++ show e2
                   | (e1, s, e2) <- transicionesMDD mdd ]
        ++ "Estado inicial: " ++ show (inicialMDD mdd) ++ "\n"
        ++ "Estados finales: " ++ show (finalesMDD mdd) ++ "\n"
        ++ "Estados finales con etiquetas: " ++ show (finalesConEtiquetasMDD mdd) ++ "\n"
        ++ "==========================="

{-
    Función que construye una MDD a partir de un AFD.
        Básicamente copia todo el AFD; y sólamente para el constructor
    finalesConEtiquetasMDD, se tiene una lista de duplas estadoFinal-etiqueta.
-}
construyeMDD :: AFD -> MDD
construyeMDD afd = MDD 
                {
                estadosMDD = estadosAfd afd,
                alfabetoMDD = alfabetoAfd afd,
                transicionesMDD = transicionesAfd afd,
                inicialMDD = inicialAfd afd,
                finalesMDD = finalesAfd afd,
                finalesConEtiquetasMDD = asignaEtiquetas afd
                }

-- Función que asigna las etiquetas correspondientes a los estados finales del MDD.
asignaEtiquetas :: AFD -> [(Estado, Etiqueta)]
asignaEtiquetas afd =
    let
        asignarEtiqueta estado
            | estado == 1       = (estado, "OpBool")
            | estado == 10      = (estado, "OpArit")
            | estado == 13      = (estado, "Entero")
            | estado == 26      = (estado, "Id")
            | estado == 56      = (estado, "Id")
            | estado == 57      = (estado, "Id")
            | estado == 60      = (estado, "Id")
            | estado == 71      = (estado, "Id")
            | estado == 74      = (estado, "Id")
            | otherwise         = error "Esto no deberia pasar. Un estado final no debería ser final, al menos para IMP."
    in
        map asignarEtiqueta (finalesAfd afd)    

{-
    Busca el siguiente estado dado un estado actual, un símbolo de entrada y una MDD.
        Se busca una coincidencia en las transiciones de la MDD dados un estado y un símbolo.
        Devuelve el siguiente estado, o el estado 2, dada la coincidencia.
-}
siguienteEstadoMDD :: MDD -> Int -> Char -> Maybe Int
siguienteEstadoMDD mdd estado simbolo =
    case filter (\(e1, s, _) -> e1 == estado && s == simbolo) (transicionesMDD mdd) of
        [] -> Nothing
        ((_, _, e2):_) -> Just e2 -- si llegara a haber más de una transición, se toma la primera; aunque no debería pasar.
        