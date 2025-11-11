-- Declaración del módulo Lexer
module Lexer(Token(..), lexer, armaMDD) where

-- Importamos módulos necesarios
import RE
import AFNep
import AFN
import AFD
import MDD
import MinimizacionAFD


-- Definimos el tipo de dato algebraico Token
data Token = Id String
        | Entero Int
        | Asignacion 
        | OpArit Char
        | OpBool String
        | PalabRes String
        | Delimitadores Char
        | Espacios Char
        | ComenBloque String
        | ComenLinea String
        deriving (Show, Eq)


-- Función que recibe una expresión regular y devuelve la MDD correspondiente
armaMDD :: RE -> MDD
armaMDD re = (construyeMDD (minimizarAFD (afnToAfd (eliminarEpsilonTrans (reToAFNEp re)))))


{-
    Función lexer
        Toma el código fuente y una MDD, y devuelve una lista de los
    tokens encontrados.
        Si el código fuente tiene un error léxico, se lanza un error.
        Se utiliza la función auxLexer como función auxiliar.
-}
lexer :: String -> MDD -> [Token]
lexer [] _ = []
lexer codigoFuente mdd = 
    case auxLexer mdd codigoFuente of
        Nothing -> error $ "Error léxico: no se pudo reconocer token en: " ++ take 20 codigoFuente
        Just (token, resto) -> token : lexer resto mdd
   

{-
    Función auxiliar del lexer.
        Recibe una MDD y el código fuente, y devuelve el siguiente token reconocido,
    junto con el resto del código fuente que aún no ha sido procesado.
        Si no reconoce ningún token, devuelve Nothing, y así el lexer lanzaría 
    un error.
        Se manda a llamar una función auxiliar que realiza el algoritmo de Maximal 
    Munch. Se le pasan parámetros extras necesarios para realizarlo:

        - El estado actual del MDD (inicialmente el estado inicial)
        - El último estado final alcanzado (inicialmente ninguno, -1)

        - El índice de la posición actual en el código fuente (inicialmente 0)
        - El índice de la posición en el código fuente de donde se alcanzó el último estado final (inicialmente 0)
-} 
auxLexer :: MDD -> String -> Maybe (Token, String)
auxLexer mdd codigoFuente = maximalMunch mdd codigoFuente (inicialMDD mdd) (-1) 0 0 


{-
    Función que recorre la MDD según el algoritmo de Maximal Munch.
        Recibe la MDD, el código fuente, el estado actual, el último estado final alcanzado, 
    la posición actual en el código fuente y la posición en el código fuente de donde se alcanzó 
    el último estado final.
        Devuelve el token reconocido y el resto del código fuente aún no analizado. O devuelve
    Nothing si no se reconoce ningún token.
-}
maximalMunch :: MDD -> String -> Int -> Int -> Int -> Int -> Maybe (Token, String)
maximalMunch mdd codigoFuente estadoActual ultimoEstadoFinal indiceActual indiceUltimoEstadoFinal
    -- Cuando aún hay código fuente que procesar
    | indiceActual < length codigoFuente =
        let simboloActual = codigoFuente !! indiceActual    -- Se obtiene el símbolo actual
            siguienteEstado = siguienteEstadoMDD mdd estadoActual simboloActual -- Se obtiene el siguiente estado
        in case siguienteEstado of
            Nothing -> -- Cuando no hay siguiente estado, la MDD se trabó
                case ultimoEstadoFinal of
                    -1  ->  Nothing -- Si se trabó sin pasar por un estado final, no se encontró ningún token y se manda error
                    _   ->  Just(creaToken ultimoEstadoFinal tokenCrudo mdd, restoCodigoFuente) -- Se devuelve el token reconocido hasta el último estado final
                        where
                            tokenCrudo = take indiceUltimoEstadoFinal codigoFuente  -- Se obtiene el token crudo directo del código fuente, posteriormente se convierte en tipo Token
                            restoCodigoFuente = drop indiceUltimoEstadoFinal codigoFuente   -- Se obtiene el resto del codigo fuente que no ha sido procesado aún
            Just nuevoEstado -> -- Cuando se encontró un siguiente estado en la MDD
                -- Se hace recursión actualizando los estados y los índices
                maximalMunch mdd codigoFuente nuevoEstado nuevoUltimoEstadoFinal nuevoIndiceActual nuevoIndiceUltimoEstadoFinal
                where
                    nuevoIndiceActual = indiceActual + 1
                    nuevoUltimoEstadoFinal = if nuevoEstado `elem` finalesMDD mdd then nuevoEstado else ultimoEstadoFinal
                    nuevoIndiceUltimoEstadoFinal = if nuevoEstado `elem` finalesMDD mdd then nuevoIndiceActual else indiceUltimoEstadoFinal
    -- Cuando ya no hay código fuente que procesar
    | otherwise =
        case ultimoEstadoFinal of
            -1  ->  Nothing -- Si nunca se pasó por un estado final, no se encontró ningún token y se manda error
            -- Se devuelve el último token reconocido
            _   ->  Just(creaToken ultimoEstadoFinal tokenCrudo mdd, restoCodigoFuente) 
                where
                    tokenCrudo = take indiceUltimoEstadoFinal codigoFuente
                    restoCodigoFuente = drop indiceUltimoEstadoFinal codigoFuente
    

{-
    Crea un token a partir de un estado final, un token crudo y la MDD.
        Se busca la etiqueta del estado final en la MDD y se crea el token correspondiente.
        En caso de que el estado pueda producir más de un token, se verifica qué etiqueta le 
    corresponde al token crudo.
-}
creaToken :: Int -> String -> MDD -> Token
creaToken estadoFinal token mdd =
    case lookup estadoFinal (finalesConEtiquetasMDD mdd) of
        Nothing ->  error $ "Estado sin etiqueta (estado no final): " ++ show estadoFinal
        Just etiqueta   ->  case etiqueta of
            "Id"        ->  Id token
            "Entero"    ->  Entero (read token)
            "OpArit"    ->  OpArit (primerElemento token) 
            "OpBool"
                | length token == 1 && primerElemento token `elem` ['a'..'z'] -> Id token
                | token == "0" ->  Entero 0
                | token `elem` palabrasReservadas -> PalabRes token
                | token `elem` operadoresBooleanos -> OpBool token
                | length token == 1 && primerElemento token `elem` delimitadores -> Delimitadores (primerElemento token)
                | length token == 1 && primerElemento token `elem` operadoresAritmeticos -> OpArit (primerElemento token)
                | length token == 1 && primerElemento token `elem` espacios -> Espacios (primerElemento token)
                | token == ":=" -> Asignacion
                | otherwise -> OpBool token --por si las dudas
            _ -> error $ "Etiqueta desconocida: " ++ etiqueta

-- Lista de palabras reservadas
palabrasReservadas :: [String]
palabrasReservadas = ["if", "then", "else", "while", "for"]

-- Lista de operadores booleanos
operadoresBooleanos :: [String]
operadoresBooleanos = ["<=", "==", "|", "&", "!", "true", "false"]

-- Lista de operadores aritméticos
operadoresAritmeticos :: [Char]
operadoresAritmeticos = ['+', '-', '*', '/']

-- Lista de delimitadores
delimitadores :: [Char]
delimitadores = [';', '(', ')', '{', '}']

-- Lista de espacios
espacios :: [Char]
espacios = [' ', '\t', '\n']

{-
    Función auxiliar para obtener el primer elemento de una lista.
    Útil para convertir un String "x" en un Char 'x'.
-} 
primerElemento :: [a] -> a
primerElemento []     = error "Error: se intentó obtener el primer elemento de una lista vacía"
primerElemento (x:_)  = x