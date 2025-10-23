module AFDmin (minimizarAFD) where

import AFD (AFD(..))
import Data.List (sort, nub)
import Data.Maybe (fromJust)
import Data.Set (Set, fromList, member, union)

type Estado = Int
type Par = (Estado, Estado)

minimizarAFD :: AFD -> AFD
minimizarAFD afd =
  let
    pares = paresUnicos (estadosAfd afd)
    tablaInicial = inicioTabla pares (finalesAfd afd)
    tablaFinal = equivalenciasTransicionesTabla afd pares tablaInicial
    estadosEquivalentes = agruparEquivalentes (estadosAfd afd) tablaFinal
  in
    construirAFDMinimo afd estadosEquivalentes


inicioTabla :: [Par] -> [Estado] -> Set Par
inicioTabla pares finales =
  fromList
    [ (p, q)
    | (p, q) <- pares
    , (p `elem` finales) /= (q `elem` finales) ]


paresUnicos :: [Estado] -> [Par]
paresUnicos estados =
  [ (p, q)
  | p <- estados, q <- estados, p < q ]

ordenarPar :: Par -> Par
ordenarPar (a, b) = if a < b then (a, b) else (b, a)


equivalenciasTransicionesTabla :: AFD -> [Par] -> Set Par -> Set Par
equivalenciasTransicionesTabla afd pares tabla =
  let
    transiciones = transicionesAfd afd

    iter tablaActual =
      let
        nuevosEnTabla = [ (p, q)
                 | (p, q) <- pares
                 , not (member (p, q) tablaActual)
                 , let transp = transicionesPorEstado transiciones p
                       transq = transicionesPorEstado transiciones q
                       comunes = [ (d1, d2)
                                 | (s1, d1) <- transp
                                 , (s2, d2) <- transq
                                 , s1 == s2 ]
                 , any (\(d1, d2) -> member (ordenarPar (d1, d2)) tablaActual) comunes
                 ]
        tablaNueva = union tablaActual (fromList nuevosEnTabla)
      in
        if null nuevosEnTabla
        then tablaActual
        else iter tablaNueva
  in
    iter tabla


transicionesPorEstado :: [(Estado, Char, Estado)] -> Estado -> [(Char, Estado)]
transicionesPorEstado transiciones e =
  [ (s, d) | (o, s, d) <- transiciones, o == e ]



agruparEquivalentes :: [Estado] -> Set Par -> [[Estado]]
agruparEquivalentes estados tabla =
  let
    sonEquivalentes p q = not (member (ordenarPar (p, q)) tabla)
    agrupar [] grupos = grupos
    agrupar (e:es) grupos =
      case [ g | g <- grupos, all (sonEquivalentes e) g ] of
        (g:_) -> agrupar es ((e:g) : filter (/= g) grupos)
        []    -> agrupar es ([e] : grupos)
  in
    agrupar estados []



construirAFDMinimo :: AFD -> [[Estado]] -> AFD
construirAFDMinimo afd estadosEquivalentes =
  let

    buscarClase e = fromJust $ lookup e [(x, i) | (grupo, i) <- zip estadosEquivalentes [0..], x <- grupo]

    transMin = nub [ (buscarClase o, s, buscarClase d)
                   | (o, s, d) <- transicionesAfd afd ]

    inicialMin = buscarClase (inicialAfd afd)
    finalesMin = nub [ buscarClase f | f <- finalesAfd afd ]
  in
    AFD
      { estadosAfd = [0..length estadosEquivalentes - 1]
      , alfabetoAfd = alfabetoAfd afd
      , transicionesAfd = transMin
      , inicialAfd = inicialMin
      , finalesAfd = sort finalesMin
      }