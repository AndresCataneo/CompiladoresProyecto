{
module Parser where
import RE  
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

{
-- Definimos el tipo de dato Token y la función de manejo de errores
data Token
    = TLParen | TRParen
    | TUnion
    | TKleene
    | TEpsilon
    | TEmpty      -- ""
    | TChar Char
    deriving (Show)

parseError :: [Token] -> a
parseError _ = error "Error de parseo"

-- Convertir una cadena en una lista de tokens
lexer :: String -> [Token]
lexer [] = []  
lexer ('"' : '"' : cs) = TEmpty : lexer cs  -- "" → ε
lexer (' ' : cs) = lexer cs                 -- Ignoramos espacios
lexer ('(' : cs) = TLParen : lexer cs
lexer (')' : cs) = TRParen : lexer cs
lexer ('+' : cs) = TUnion : lexer cs
lexer ('*' : cs) = TKleene : lexer cs
lexer ('ε' : cs) = TEpsilon : lexer cs

-- operadores escapados
lexer ('\\' : '+' : cs) = TChar '+' : lexer cs
lexer ('\\' : '*' : cs) = TChar '*' : lexer cs

-- cualquier otro caracter lo consideramos un símbolo
lexer (c : cs) = TChar c : lexer cs
}

