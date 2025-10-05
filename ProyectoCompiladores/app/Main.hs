module Main (main) where

import Parser
--import RE
import AFNep
import System.Environment (getArgs)
--import System.IO (readFile)

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
            let afn = reToAFNEp ast
            putStrLn "\nAFN-ε resultante:"
            print afn
        _ -> putStrLn "Uso: compilador <archivo.txt>"