module Lexer() where

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
                deriving (Show)

lexer :: String -> [Token]
lexer _ = []