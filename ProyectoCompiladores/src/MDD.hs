module MDD (MDD(..), construyeMDD) where

import AFD 

type Etiqueta = String
type Estado = Int
type Transicion = (Estado, Char, Estado)

data MDD = MDD
    {
        estados :: [Estado],
        alfabeto :: [Char],
        transiciones :: [Transicion],
        inicial :: Estado,
        finales :: [Estado],
        finalesConEtiquetas :: [(Estado, Etiqueta)]  
    } deriving (Show)


construyeMDD :: AFD -> MDD
construyeMDD afd = MDD 
                {
                estados = estadosAfd afd,
                alfabeto = alfabetoAfd afd,
                transiciones = transicionesAfd afd,
                inicial = inicialAfd afd,
                finales = finalesAfd afd,
                finalesConEtiquetas = asignaEtiquetas afd
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





-- type Estado = Int
-- type Trans = (Estado, Char, Estado)

-- data AFD = AFD {estados :: [Estado], 
--         alfabeto :: [Char], 
--         transiciones :: [Trans], 
--         inicial :: Estado, 
--         final :: [Estado]}
--         deriving (Show)


-- ultimoEstado :: AFD -> String -> Int
-- ultimoEstado afd s = if acepta afd s then
--                 transita (transicionesAfd afd) (inicialAfd afd) s
--                 else -1

-- data MDD = MDD {}
--         deriving (Show)