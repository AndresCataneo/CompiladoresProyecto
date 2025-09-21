module Main where

import Parser
import RE
import System.Environment (getArgs)
import System.IO (readFile)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contenido <- readFile filename
            let tokens = lexer contenido
            let ast = parseRE tokens
            putStrLn "ER resultante:"
            print ast
        _ -> putStrLn "Uso: compilador <archivo.txt>"