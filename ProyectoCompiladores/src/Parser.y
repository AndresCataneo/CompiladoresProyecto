{
module Parser where
import RE
import Data.Char (chr)  
}

--Directiva para Happy
%name parseRE
--Token Type
%tokentype { Token }
%error { parseError }

--Definimos los tokens para cada símbolo de la expresión regular de RE
%token
    '('       { TLParen }
    ')'       { TRParen }
    '+'       { TUnion }
    '*'       { TKleene }
    'ε'       { TEpsilon } 
    emptyStr  { TEmpty }     -- ""
    char      { TChar $$ }
    range     { TRange $$ }

--Definimos la precedencia y asociatividad de los operadores
%right '+'   -- Union (más bajo)
%left '*'    -- Kleene (más alto)

%%

-- Símbolo inicial
S
    : Exp                { $1 }
    |                    { Vacio }    -- archivo vacío ≡ ∅

Exp
    : Exp '+' Term       { Union $1 $3 }
    | Term               { $1 }

Term
    : Term Factor        { Concatena $1 $2 } 
    | Factor             { $1 }

Factor
    : Atom '*'           { Estrella $1 }
    | Atom               { $1 }

Atom
    : '(' Exp ')'        { $2 }
    | 'ε'                { Epsilon }
    | emptyStr           { Epsilon }    -- "" ≡ ε
    | char               { Symbol $1 }
    | range              { $1 }

{
-- Definimos el tipo de dato Token y la función de manejo de errores
data Token
    = TLParen | TRParen
    | TUnion
    | TKleene
    | TEpsilon
    | TEmpty      -- ""
    | TChar Char
    | TRange RE
    deriving (Show)

parseError :: [Token] -> a
parseError _ = errorWithoutStackTrace "Error de parseo. Se debe ingresar una expresión regular válida.\nEjemplos: ab, a+b, (a+b)*, a*, ε, \"\"."

rangeToRE :: Char -> Char -> RE
rangeToRE start end = foldr1 Union (map Symbol [start..end])

negatedRangeToRE :: Char -> RE
negatedRangeToRE c = 
    let allPrintable = [chr 9, chr 10] ++ [chr 32 .. chr 126] 
        validChars = filter (/= c) allPrintable
    in foldr1 Union (map Symbol validChars)


-- Convertir una cadena en una lista de tokens
lexer :: String -> [Token]
lexer [] = []  
lexer ('"' : '"' : cs) = TEmpty : lexer cs  -- "" → ε
lexer (' ' : cs) = lexer cs                   
lexer ('\r' : cs) = lexer cs        
lexer ('(' : cs) = TLParen : lexer cs
lexer (')' : cs) = TRParen : lexer cs
lexer ('+' : cs) = TUnion : lexer cs
lexer ('*' : cs) = TKleene : lexer cs
lexer ('ε' : cs) = TEpsilon : lexer cs
lexer ('[' : c1 : '-' : c2 : ']' : cs) = 
--Rangos de caracteres de [a-z], [0-9]
    TRange (rangeToRE c1 c2) : lexer cs

-- Rangos negados con caracteres escapados como [^n], [^ ]
lexer ('[' : '^' : '\\' : esc : ']' : cs) =
    let c = case esc of
                'n' -> '\n'
                ' ' -> ' '
                't' -> '\t'
                _   -> esc
    in TRange (negatedRangeToRE c) : lexer cs

-- Rangos negados simples como[^a], [^b]
lexer ('[' : '^' : c : ']' : cs) =
    TRange (negatedRangeToRE c) : lexer cs

-- operadores escapados
lexer ('\\' : '+' : cs) = TChar '+' : lexer cs
lexer ('\\' : '*' : cs) = TChar '*' : lexer cs
lexer ('\\' : '(' : cs) = TChar '(' : lexer cs
lexer ('\\' : ')' : cs) = TChar ')' : lexer cs
lexer ('\\' : ' ' : cs) = TChar ' ' : lexer cs
lexer ('\\' : 'n' : cs) = TChar '\n' : lexer cs
lexer ('\\' : 't' : cs) = TChar '\t' : lexer cs
lexer ('\\' : '=' : cs) = TChar '=' : lexer cs
lexer ('\\' : '-' : cs) = TChar '-' : lexer cs
lexer ('\\' : '/' : cs) = TChar '/' : lexer cs
-- cualquier otro caracter lo consideramos un símbolo
lexer (c : cs) = TChar c : lexer cs
}

