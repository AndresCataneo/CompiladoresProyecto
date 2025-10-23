module MDD (MDD(..), construyeMDD) where

type Estado = Int
type Trans = (Estado, Char, Estado)

data AFD = AFD {estados :: [Estado], 
        alfabeto :: [Char], 
        transiciones :: [Trans], 
        inicial :: Estado, 
        final :: [Estado]}
        deriving (Show)


data MDD = MDD {}
        deriving (Show)

construyeMDD :: AFD -> MDD
construyeMDD _ = MDD {}

