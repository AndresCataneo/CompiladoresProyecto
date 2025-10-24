module Lexer(Token(..), lexer, armaMDD) where

import RE
import AFNep
import AFN
import AFD
import AFDmin
import MDD
       
data Token = Id String
        | Entero Int
        | Asignacion 
        | OpArit Char
        | OpBool String
        | PalabRes String
        | Delimitadores Char
        | Espacios String
        | ComenBloque String
        | ComenLinea String
        deriving (Show, Eq)

-- Construye el MDD a partir de una expresión regular
armaMDD :: RE -> MDD
armaMDD re = (construyeMDD (minimizarAFD (afnToAfd (eliminarEpsilonTrans (reToAFNEp re)))))

-- Función principal del lexer
-- Recibe el código fuente y retorna una lista de tokens
lexer :: String -> MDD -> [Token]
lexer [] _ = []
lexer input mdd = 
    case maximalMunch mdd input of
        Nothing -> error $ "Error léxico: no se pudo reconocer token en: " ++ take 20 input
        Just (token, resto) -> token : lexer resto mdd
   

-- Implementación del algoritmo de Maximal Munch con retroceso
maximalMunch :: MDD -> String -> Maybe (Token, String)
maximalMunch mdd input = 
    runMDD mdd input (inicialMDD mdd) Nothing 0 0

-- Ejecuta el MDD con la política de maximal munch
-- Estados: estadoActual, ultimoEstadoFinal, posicionUltimoFinal, posicionActual
runMDD :: MDD -> String -> Int -> Maybe (Int, Int) -> Int -> Int -> Maybe (Token, String)
runMDD mdd input estadoActual ultimoFinal posUltimoFinal posActual
    -- Si ya no hay más entrada
    | posActual >= length input =
        case ultimoFinal of
            Nothing -> Nothing  -- No se reconoció ningún token
            Just (estadoFinal, pos) -> 
                let lexema = take pos input
                    resto = drop pos input
                in Just (crearToken estadoFinal lexema mdd, resto)
    
    -- Si hay más entrada, intentamos seguir
    | otherwise =
        let char = input !! posActual
            siguienteEdo = siguienteEstadoMDD mdd estadoActual char
        in case siguienteEdo of
            Nothing -> -- No hay transición, retrocedemos
                case ultimoFinal of
                    Nothing -> Nothing
                    Just (estadoFinal, pos) ->
                        let lexema = take pos input
                            resto = drop pos input
                        in Just (crearToken estadoFinal lexema mdd, resto)
            
            Just nuevoEstado ->
                let nuevaPosicion = posActual + 1
                    -- Verificamos si el nuevo estado es final
                    nuevoUltimoFinal = 
                        if nuevoEstado `elem` finalesMDD mdd
                        then Just (nuevoEstado, nuevaPosicion)
                        else ultimoFinal
                    nuevaPosUltimoFinal = 
                        if nuevoEstado `elem` finalesMDD mdd
                        then nuevaPosicion
                        else posUltimoFinal
                in runMDD mdd input nuevoEstado nuevoUltimoFinal nuevaPosUltimoFinal nuevaPosicion

-- Busca la siguiente transición en el MDD
siguienteEstadoMDD :: MDD -> Int -> Char -> Maybe Int
siguienteEstadoMDD mdd estado char =
    case filter (\(o, c, _) -> o == estado && c == char) (transicionesMDD mdd) of
        [] -> Nothing
        ((_, _, destino):_) -> Just destino --por si llegara a haber más de una transición

-- Crea un token basado en el estado final y el lexema
crearToken :: Int -> String -> MDD -> Token
crearToken estadoFinal lexema mdd =
    case lookup estadoFinal (finalesConEtiquetasMDD mdd) of
        Nothing -> error $ "Estado final sin etiqueta: " ++ show estadoFinal
        Just etiqueta -> case etiqueta of
            "Id" -> 
                -- Verificamos si es una palabra reservada
                if lexema `elem` palabrasReservadas
                then PalabRes lexema
                else Id lexema
            "Entero" -> Entero (read lexema)
            "Asignacion" -> Asignacion
            "OpArit" -> case lexema of
                (c:_) -> OpArit c
                [] -> error "OpArit: lexema vacío"
            "OpBool" -> OpBool lexema
            "PalabRes" -> PalabRes lexema
            "Delimitadores" -> case lexema of
                (c:_) -> Delimitadores c
                [] -> error "Delimitadores: lexema vacío"
            "Espacios" -> Espacios lexema
            "ComenBloque" -> ComenBloque lexema
            "ComenLinea" -> ComenLinea lexema
            _ -> error $ "Etiqueta desconocida: " ++ etiqueta

-- Lista de palabras reservadas del lenguaje IMP
palabrasReservadas :: [String]
palabrasReservadas = ["if", "then", "else", "while", "for"]