module Main (main) where

import Parser
import AFNep
import System.Environment (getArgs)
import Data.List (intercalate, dropWhileEnd, isPrefixOf)
import AFN
import AFD
--import AFDmin
import MinimizacionAFD(minimizarAFD)
import Lexer (armaMDD, lexer)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filenameER, filenameInput] -> do
            -- Leer el archivo con las expresiones regulares
            contenidoER <- readFile filenameER
            let preproc = preprocesar contenidoER
            let tokens = Parser.lexer preproc  -- lexer del parser (para la ER)
            let ast = parseRE tokens
            putStrLn "ER resultante:"
            print ast
            
            let afne = reToAFNEp ast
            putStrLn "\nAFNe resultante:"
            print afne 
            putStrLn "El numero de estados del AFNe es:"
            print (length (estados afne))
            
            let afn = eliminarEpsilonTrans afne
            putStrLn "\nAFN resultante:"
            print afn
            
            let afd = afnToAfd afn
            putStrLn "\nAFD no min resultante:"
            print afd
            
            let afdmin = minimizarAFD afd
            putStrLn "\nAFD min resultante:"
            print afdmin
            
            -- Construir el MDD
            let mdd = armaMDD ast
            putStrLn "\nMDD resultante:"
            print mdd
            
            -- Leer el archivo de entrada (código a tokenizar)
            codigoFuente <- readFile filenameInput
            putStrLn "\n========================================="
            putStrLn "Código fuente a tokenizar:"
            putStrLn "========================================="
            putStrLn codigoFuente
            putStrLn "========================================="
            
            -- Aplicar el lexer al código fuente
            let codigoFuenteSinComentarios = quitarComentarios codigoFuente
            let tokensResultado = Lexer.lexer codigoFuenteSinComentarios mdd
            putStrLn "\nTokens reconocidos:"
            putStrLn "========================================="
            mapM_ print tokensResultado
            
        [filename] -> do
            -- Modo original: solo procesar las expresiones regulares
            contenido <- readFile filename
            let preproc = preprocesar contenido
            let tokens = Parser.lexer preproc
            let ast = parseRE tokens
            putStrLn "ER resultante:"
            print ast
            
            let afne = reToAFNEp ast
            putStrLn "AFNe resultante:"
            print afne 
            putStrLn "El numero de estados del AFNe es:"
            print (length (estados afne))
            
            let afn = eliminarEpsilonTrans afne
            putStrLn "AFN resultante:"
            print afn
            
            let afd = afnToAfd afn
            putStrLn "\nAFD no min resultante:"
            print afd
            
            let afdmin = minimizarAFD afd
            putStrLn "\nAFD min resultante:"
            print afdmin
            
            let mdd = armaMDD ast
            putStrLn "\nMDD resultante:"
            print mdd
            
        _ -> putStrLn "Uso:\n  compilador <archivoER.txt>  # Solo procesar ER\n  compilador <archivoER.txt> <archivoInput.txt>  # Procesar ER y tokenizar"

-- Función auxiliar que elimina espacios en blanco al inicio y final de cada línea
trim :: String -> String
trim s = dropWhileEnd (`elem` [' ', '\t', '\r']) (dropWhile (`elem` [' ', '\t', '\r']) s)

-- Función que preprocesa el contenido del archivo de entrada donde se definen las ER
{-
Eliminamos líneas vacías y espacios en blanco al inicio y final de cada línea
y tomamos las cadenas que están después del '=' en cada línea para luego unirlas con '+'
-}
preprocesar :: String -> String
preprocesar contenido =
    let
        todasLasLineas = lines contenido
        lineasLimpias = map trim todasLasLineas
        lineasNoVacias = filter (\l -> not (null l)) lineasLimpias
        --Tomamos las expresiones regulares que están después del '='
        expresiones = [ trim (drop 1 (dropWhile (/= '=') l))
                        | l <- lineasNoVacias, '=' `elem` l ]
        resultado = intercalate "+" expresiones
    in
        resultado

-- Elimina comentarios // de una línea
quitarComentarioLinea :: String -> String
quitarComentarioLinea = quitarDesde "//"
  where
    quitarDesde _ [] = []
    quitarDesde patron str@(x:xs)
      | patron `isPrefixOf` str = []
      | otherwise = x : quitarDesde patron xs

-- Elimina comentarios // de todo el archivo
quitarComentarios :: String -> String
quitarComentarios texto = 
  let lineasOriginales = lines texto --Divide el string en una lista de líneas string
      lineasProcesadas = map quitarComentarioLinea lineasOriginales --Quita los comentarios en cada elemento de la lista de lineas
  in unlines lineasProcesadas --Convierte la lista de líneas en un solo string separado por saltos de línea