
-- EXPRESIONES REGULARES
module RE (RE(..), stringToRE, digito, numNat, var, identif, entero, asignacion, operadorAri, operadorBool, 
    delimitadores, palabResReservadas, espaciosBlanco, componentesLexicos, comentarioLinea) where

--import Data.Set (toList, fromList)

--Abstraccion de RE
data RE = Vacio
        | Epsilon
        | Symbol Char
        | Concatena RE RE 
        | Union RE RE
        | Estrella RE
        deriving (Eq)
--Mostrar expresiones regulares (Version simple)
{-
instance Show RE where
    show Vacio = "∅"
    show Epsilon = "ε"
    show (Symbol c) = [c]
    show (Concatena e1 e2) = "(" ++ show e1 ++ show e2 ++ ")"
    show (Union e1 e2) = "(" ++ show e1++ " U " ++ show e2 ++ ")"
    show (Estrella e) = "(" ++ show e ++ ")*"
    -}

--Mostrar exp regulares (Eliminamos paréntesis innecesarios)
instance Show RE where
    show = showPrec 0

-- showPrec :: nivel de contexto -> RE -> String
showPrec :: Int -> RE -> String
showPrec _ Vacio = "∅"
showPrec _ Epsilon = "ε"
showPrec _ (Symbol ' ') = " "                    -- Mostrar espacio
showPrec _ (Symbol '\n') = "\\n"                 -- Mostrar \n
showPrec _ (Symbol '\t') = "\\t"                 -- Mostrar \t  
showPrec _ (Symbol '\r') = "\\r"                 -- Mostrar \r
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

--Funcion que transfroma una cadena en una expresion regular de concatenaciones
stringToRE :: String -> RE
stringToRE [] = Epsilon
stringToRE [c] = Symbol c
stringToRE (c:cs) = Concatena (Symbol c) (stringToRE cs)

--ER que representa los enteros positivos (0-9)
digito :: RE
digito = foldr1 Union (map Symbol ['0'..'9'])

--ER que Representa un numero de 1 a 9
numNat :: RE
numNat = foldr1 Union (map Symbol ['1'..'9'])

-- ER que representa cualquier letra (mayuscula o minuscula)
var :: RE
var = foldr1 Union (map Symbol (['a'..'z'] ++ ['A'..'Z']))

-- ER que epresenta una letra seguida de cualquier combinación de letras y dígitos
identif :: RE
identif = Concatena var (Estrella (Union var digito))

--ER que represenenta enteros (positivos y negativos) donde no puede haber ceros a la izquierda. 0 U [1-9][0-9]* U -[1-9][0-9]*
entero :: RE
entero = Union (Symbol '0') (Union (Concatena (numNat) (Estrella digito)) (Concatena (Symbol '-') (Concatena (numNat) (Estrella digito))))

--ER que representa la asignacion a una variable
asignacion :: RE
asignacion = Concatena (Symbol ':') (Symbol '=')

--OperadoresAritmeticos
operadorAri :: RE
operadorAri = Union (Symbol '+') (Union (Symbol '-') (Union (Symbol '*') (Symbol '/')))

--Expresiones booleanas 
operadorBool :: RE
operadorBool= Union (Union (Symbol '&') (Symbol '|')) (Union (Symbol '!') (Union (Concatena (Symbol '=') (Symbol '=')) (Union (Concatena (Symbol '<') (Symbol '='))(Union (stringToRE "true") (stringToRE "false")))))

--ER de Delimitadores. ; tiene dos usos, uno como fin de instruccion y otro como operador de secuencia de instrucciones. Esto se maneja en el analizador sintactico
delimitadores :: RE
delimitadores = foldr1 Union (map Symbol ['{','}','(',')',';'])

--ER de palabras reservadas de condicionales y ciclos
palabResReservadas :: RE
palabResReservadas = Union (stringToRE "if") (Union (stringToRE "else") (Union (stringToRE "while") (stringToRE "for")))

--ER de los espacios en blanco
espaciosBlanco :: RE
espaciosBlanco = Estrella (Union (Symbol ' ') (Union (Symbol '\n') (Symbol '\t')))

--Funcion auxiliar que define todo nuestos componentes lexicos 
componentesLexicos :: RE
componentesLexicos = Union (var) (Union (digito) (Union (entero) (Union (asignacion) (Union (operadorAri) (Union (operadorBool) (Union (delimitadores) (Union (palabResReservadas) (espaciosBlanco))))))))

--ER de comentarios donde empieza con /* y terminan con */
comentarioLinea :: RE
comentarioLinea = Concatena (stringToRE "/*") (Concatena (Estrella componentesLexicos) (stringToRE "*/"))
