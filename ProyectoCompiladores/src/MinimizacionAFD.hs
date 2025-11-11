module MinimizacionAFD(minimizarAFD) where

import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import AFD(AFD(..), Transicion)


-- Par de estados en orden canónico
data ParEstados = ParEstados Int Int deriving (Eq, Ord, Show)

-- Crear par en orden canónico (el menor primero)
crearPar :: Int -> Int -> ParEstados
crearPar a b = if a <= b then ParEstados a b else ParEstados b a

-- Tabla de distinguibilidad
type TablaDistinguibilidad = Map ParEstados Bool

-- Mapa de transiciones: (estado, símbolo) -> estado destino
type MapaTransiciones = Map (Int, Char) Int

-- Construir mapa de transiciones desde lista
construirMapaTransiciones :: [Transicion] -> MapaTransiciones
construirMapaTransiciones = foldr insertarTransicion Map.empty
  where
    insertarTransicion (origen, simbolo, destino) = Map.insert (origen, simbolo) destino

-- Obtener transición desde un estado con un símbolo
obtenerTransicion :: MapaTransiciones -> Int -> Char -> Maybe Int
obtenerTransicion mapa estado simbolo = Map.lookup (estado, simbolo) mapa

-- Inicializar tabla (marcar pares final vs no-final)
inicializarTabla :: [Int] -> [Int] -> TablaDistinguibilidad
inicializarTabla estados finales = Map.fromList [
    (crearPar s1 s2, esFinal s1 /= esFinal s2)
    | s1 <- estados, s2 <- estados, s1 < s2
    ]
  where
    esFinal s = s `elem` finales

-- Marcado iterativo
marcarDistinguibles :: TablaDistinguibilidad -> [Int] -> [Char] -> MapaTransiciones -> TablaDistinguibilidad
marcarDistinguibles tabla estados alfabeto mapaTransiciones = 
    if tabla == nuevaTabla
    then tabla
    else marcarDistinguibles nuevaTabla estados alfabeto mapaTransiciones
  where
    nuevaTabla = foldr procesarPar tabla pares
    pares = [(s1, s2) | s1 <- estados, s2 <- estados, s1 < s2]
    
    procesarPar (s1, s2) t =
        let par = crearPar s1 s2
            yaDistinguible = Map.findWithDefault False par t
        in if yaDistinguible
           then t
           else verificarSimbolo s1 s2 t par
    
    verificarSimbolo s1 s2 t par =
        let esDistinguible = any (sonTransicionesDistinguibles t s1 s2) alfabeto
        in if esDistinguible
           then Map.insert par True t
           else t
    
    sonTransicionesDistinguibles t s1 s2 simbolo =
        case (obtenerTransicion mapaTransiciones s1 simbolo, 
              obtenerTransicion mapaTransiciones s2 simbolo) of
            (Nothing, Nothing) -> False
            (Nothing, Just _)  -> True
            (Just _, Nothing)  -> True
            (Just t1, Just t2) -> 
                if t1 == t2
                then False
                else Map.findWithDefault False (crearPar t1 t2) t

-- Crear particiones (estados equivalentes)
-- Union-Find para agrupar estados
type UnionFind = Map Int Int

inicializarUnionFind :: [Int] -> UnionFind
inicializarUnionFind estados = Map.fromList [(s, s) | s <- estados]

find :: UnionFind -> Int -> (Int, UnionFind)
find uf estado =
    case Map.lookup estado uf of
        Nothing -> (estado, uf)
        Just padre ->
            if padre == estado
            then (estado, uf)
            else let (raiz, uf') = find uf padre
                 in (raiz, Map.insert estado raiz uf')

union :: UnionFind -> Int -> Int -> UnionFind
union uf s1 s2 =
    let (r1, uf1) = find uf s1
        (r2, uf2) = find uf1 s2
    in if r1 == r2
       then uf2
       else Map.insert r1 r2 uf2

crearParticiones :: [Int] -> TablaDistinguibilidad -> [[Int]]
crearParticiones estados tabla = Map.elems grupos
  where
    -- Unir estados no distinguibles
    ufFinal = foldr unirSiEquivalentes (inicializarUnionFind estados) pares
    pares = [(s1, s2) | s1 <- estados, s2 <- estados, s1 < s2]
    
    unirSiEquivalentes (s1, s2) uf =
        let distinguible = Map.findWithDefault False (crearPar s1 s2) tabla
        in if distinguible then uf else union uf s1 s2
    
    -- Agrupar por representante
    grupos = foldr agrupar Map.empty estados
    agrupar estado acc =
        let (repr, _) = find ufFinal estado
        in Map.insertWith (++) repr [estado] acc

-- Construir AFD minimizado
minimizarAFD :: AFD -> AFD
minimizarAFD (AFD estados alfabeto transiciones inicial finales) =
    let mapaTransiciones = construirMapaTransiciones transiciones
        tablaInicial = inicializarTabla estados finales
        tablaFinal = marcarDistinguibles tablaInicial estados alfabeto mapaTransiciones
        particiones = crearParticiones estados tablaFinal
        
        -- Mapear cada estado antiguo a su representante
        mapaRepresentante = Map.fromList [
            (e, primerElemento grupo) | grupo <- particiones, e <- grupo
            ]
        
        -- Nuevos estados (representantes)
        nuevosEstados = map primerElemento particiones
        
        -- Nuevo estado inicial
        nuevoInicial = mapaRepresentante Map.! inicial
        
        -- Nuevos estados finales
        nuevosFinales = nub [mapaRepresentante Map.! f | f <- finales]
        
        -- Nuevas transiciones
        nuevasTransiciones = nub [
            (mapaRepresentante Map.! origen, simbolo, mapaRepresentante Map.! destino)
            | (origen, simbolo, destino) <- transiciones
            ]
    in AFD nuevosEstados alfabeto nuevasTransiciones nuevoInicial nuevosFinales

{-
    Función auxiliar para obtener el primer elemento de una lista.
    Útil para convertir un String "x" en un Char 'x'.
-} 
primerElemento :: [a] -> a
primerElemento []     = error "Error: se intentó obtener el primer elemento de una lista vacía"
primerElemento (x:_)  = x