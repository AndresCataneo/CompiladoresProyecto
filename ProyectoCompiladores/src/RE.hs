module RE (RE(..)) where

--Abstraccion de RE
data RE = Vacio
        | Epsilon
        | Symbol Char
        | Concatena RE RE 
        | Union RE RE
        | Estrella RE
        deriving (Eq)

--Mostrar exp regulares (Eliminamos paréntesis innecesarios)
instance Show RE where
    show = showPrec 0

-- showPrec :: nivel de contexto -> RE -> String
showPrec :: Int -> RE -> String
showPrec _ Vacio = "∅"
showPrec _ Epsilon = "ε"
showPrec _ (Symbol ' ') = " "                    
showPrec _ (Symbol '\n') = "\\n"                
showPrec _ (Symbol '\t') = "\\t"                 
showPrec _ (Symbol '\r') = "\\r"                 
showPrec _ (Symbol c) = [c]

showPrec p (Concatena e1 e2) =
    let s = showPrec 2 e1 ++ showPrec 2 e2
    in if p > 2 then "(" ++ s ++ ")" else s

showPrec p (Union e1 e2) =
    let s = showPrec 1 e1 ++ " U " ++ showPrec 1 e2
    in if p > 1 then "(" ++ s ++ ")" else s

showPrec p (Estrella e) =
    let s = showPrec 3 e ++ "*"
    in if p > 2 then "(" ++ s ++ ")" else s
