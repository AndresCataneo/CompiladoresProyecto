module MDD (MDD(..), construyeMDD) where

import AFD (AFD(..), transita, acepta)

-- type Estado = Int
-- type Trans = (Estado, Char, Estado)

-- data AFD = AFD {estados :: [Estado], 
--         alfabeto :: [Char], 
--         transiciones :: [Trans], 
--         inicial :: Estado, 
--         final :: [Estado]}
--         deriving (Show)



ultimoEstado :: AFD -> String -> Int
ultimoEstado afd s = if acepta afd s then
                transita (transicionesAfd afd) (inicialAfd afd) s
                else -1


data MDD = MDD {}
        deriving (Show)

construyeMDD :: AFD -> MDD
construyeMDD _ = MDD {}

