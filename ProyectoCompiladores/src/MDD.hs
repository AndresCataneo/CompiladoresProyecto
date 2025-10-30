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
    } 

instance Show MDD where
    show mdd =
        "Estados: " ++ show (estadosMDD mdd) ++ "\n" ++
        "Alfabeto: " ++ show (alfabetoMDD mdd) ++ "\n" ++
        "Transiciones:\n" ++ concatMap mostrarTransicion (transicionesMDD mdd) ++
        "Estado inicial: " ++ show (inicialMDD mdd) ++ "\n" ++
        "Estados finales: " ++ show (finalesMDD mdd) ++ "\n" ++
        "Estados finales con etiquetas: " ++ show (finalesConEtiquetasMDD mdd)
      where
        mostrarTransicion (from, c, to) = "  " ++ show from ++ " -" ++ [c] ++ "-> " ++ show to ++ "\n"


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
            | estado == 4       = (estado, "OpArit")
            | estado == 3       = (estado, "Entero")
            | estado == 0       = (estado, "Id")
        --  aquí puede haber más casos
            | otherwise         = error "Esto no deberia pasar. Un estado final no debería ser final, al menos para IMP."
    in
        map asignarEtiqueta (finalesAfd afd)    
        