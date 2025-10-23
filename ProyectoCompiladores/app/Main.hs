module Main (main) where

import Parser
import AFNep
import System.Environment (getArgs)
import Data.List (intercalate, dropWhileEnd)
import AFN
import AFD

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contenido <- readFile filename
            let preproc = preprocesar contenido
            let tokens = lexer preproc
            let ast = parseRE tokens
            putStrLn "ER resultante:"
            print ast
            let afne = reToAFNEp ast
            putStrLn "AFNe resultante:"
            print afne 
            let afn = eliminarEpsilonTrans afne
            putStrLn "AFN resultante:"
            print afn --lo que tarda es imprimir, pero no el cálculo
            let afd = afnToAfd afn
            putStrLn "\nAFD no min resultante:"
            print afd
        _ -> putStrLn "Uso: compilador <archivo.txt>"

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