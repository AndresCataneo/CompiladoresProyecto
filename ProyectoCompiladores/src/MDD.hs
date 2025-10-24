module MDD (MDD(..), construyeMDD) where

import AFD 

type Etiqueta = String
type Estado = Int
type Transicion = (Estado, Char, Estado)

data MDD = MDD
    {
        estadosMDD :: [Estado],
        alfabetoMDD :: [Char],
        transicionesMDD :: [Transicion],
        inicialMDD :: Estado,
        finalesMDD :: [Estado],
        finalesConEtiquetasMDD :: [(Estado, Etiqueta)]  
    } deriving (Show)


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

asignaEtiquetas :: AFD -> [(Estado, Etiqueta)]
asignaEtiquetas afd =
    let
        asignarEtiqueta estado
            | estado == 1       = (estado, "Id")
            | estado == 2       = (estado, "Entero")
            | estado == 3       = (estado, "Asignacion")
            | estado == 4       = (estado, "OpArit")
            | estado == 5       = (estado, "OpBool")
            | estado == 6       = (estado, "PalabRes")
            | estado == 7       = (estado, "Delimitadores")
            | estado == 8       = (estado, "Espacios")
            | estado == 9       = (estado, "ComenBloque")
            | estado == 10      = (estado, "ComenLinea")
        --     aquí debe haber más casos
        --              ...
            | otherwise         = error "Esto no deberia pasar. Un estado final no debería ser final, al menos para IMP."
    in
        map asignarEtiqueta (finalesAfd afd)    
        