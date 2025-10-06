module AFNep (AFNEp(..), reToAFNEp, sigma) where

import RE 
import Data.Set (toList, fromList)

--Funcion que obtiene el alfabeto de una expresion regular. Esto es para definir el alfabeto de nuestro lenguaje en el AFN-ε
sigma :: RE -> [Char]
sigma Vacio = []
sigma Epsilon = []
sigma (Symbol c) = [c]
sigma (Concatena e1 e2) = toList $ fromList (sigma e1 ++ sigma e2)
sigma (Union e1 e2) = toList $ fromList (sigma e1 ++ sigma e2)
sigma (Estrella e) = sigma e

--Transicion epsilon
{-
Recibe un estado de origen, un símbolo (Maybe Char) y una lista de estados destino 
Si el símbolo es Nothing, es una transición epsilon
Si el símbolo es Just c, es una transición con un simbolo del alfabeto
-}
type TransEp = (Int, Maybe Char, [Int])

--Estado
type Estado = Int

--AFN con transiciones epsilon
{-
En particular nuestros estados finales serán una lista de estados, ya que en la construcción de la unión
podemos tener más de un estado final
-}
data AFNEp = AFNEp {estados :: [Estado], 
        alfabeto :: [Char], 
        transiciones :: [TransEp], 
        inicial :: Estado, 
        final :: [Estado]}
        deriving (Show)

--Traducción de expresiones regulares a AFN con transiciones epsilon
{-
Funcion Principal que recibe una expresión regular de nuestro lenguaje y un estado inicial y devuelve el AFN-ε correspondiente
El estado inicial se usa para numerar los estados del AFN-ε de manera única
-}
reToAFNEp :: RE -> AFNEp
reToAFNEp e = reToAFNEpAux e 0

--Función auxiliar que recibe una expresión regular y un estado inicial y devuelve el AFN-ε correspondiente a la expresión regular
reToAFNEpAux :: RE -> Estado -> AFNEp
-- Caso vacío
{-
Sucede cuando el archivo de entrada está vacío, por lo que el lenguaje es el conjunto vacío
Devolvemos un AFN-ε con dos estados y sin transiciones
-}
reToAFNEpAux Vacio n = AFNEp {estados = [n, n+1],
                                alfabeto = [],
                                transiciones = [],
                                inicial = n,
                                final = [n+1]}
-- Caso Epsilon
{-
Sucede cuando la expresión regular es ε, contiene a ε o "".
Devolvemos un AFN-ε con dos estados y una transición ε del estado inicial al estado final
-}
reToAFNEpAux Epsilon n = AFNEp {estados = [n, n+1],
                                alfabeto = [],
                                transiciones = [(n, Nothing, [n+1])],
                                inicial = n,
                                final = [n+1]}
--Caso base, un símbolo
{-
Sucede cuando la expresión regular es un símbolo del alfabeto.
Devolvemos un AFN-ε con dos estados y una transición con el símbolo del estado inicial al estado final
-}
reToAFNEpAux (Symbol c) n = AFNEp {estados = [n, n+1],
                                alfabeto = [c],
                                transiciones = [(n, Just c, [n+1])],
                                inicial = n,
                                final = [n+1]}
--Caso union
{-Sucede cuando la expresión regular es una unión de dos expresiones regulares.
Devolvemos un AFN-ε con un nuevo estado inicial que tiene transiciones ε a los estados iniciales de
los AFN-ε correspondientes a las dos expresiones regulares, y los estados finales son la unión de los estados finales
de los dos AFN-ε.
-}
reToAFNEpAux (Union m1 m2) n = AFNEp {estados = [n] ++ estados afn1 ++ estados afn2,
                                alfabeto = toList $ fromList (alfabeto afn1 ++ alfabeto afn2),
                                transiciones = (transiciones afn1) ++ (transiciones afn2) ++
                                                [ (n, Nothing, [inicial afn1, inicial afn2])],
                                inicial = n,
                                final = final afn1 ++ final afn2 }
        where 
            afn1 = reToAFNEpAux m1 (n+1)
            edoIniM2 = maximum (estados afn1) + 1 
            afn2 = reToAFNEpAux m2 edoIniM2
--Caso concatenacion
{-Sucede cuando la expresión regular es una concatenación de dos expresiones regulares.
Devolvemos un AFN-ε que conecta los estados finales del primer AFN-ε con el estado inicial del segundo AFN-ε
mediante transiciones ε.
-}
reToAFNEpAux (Concatena m1 m2) n = AFNEp {estados = estados afn1 ++ estados afn2,
                                alfabeto = toList $ fromList (alfabeto afn1 ++ alfabeto afn2),
                                transiciones = (transiciones afn1) ++ (transiciones afn2) ++
                                                [(f, Nothing, [inicial afn2]) | f <- final afn1],
                                inicial = inicial afn1,
                                final = final afn2 }
        where               
            afn1 = reToAFNEpAux m1 n
            edoIniM2 = maximum (estados afn1) + 1 
            afn2 = reToAFNEpAux m2 edoIniM2
--Caso estrella
{-Sucede cuando la expresión regular contiene la estrella de kleene.
Devolvemos un AFN-ε donde el estado inicial es también un estado final, y tiene transiciones ε de los estados finales al estado inicial
-}
reToAFNEpAux (Estrella m) n = AFNEp {estados = estados afn1,
                                alfabeto = alfabeto afn1,
                                transiciones = (transiciones afn1) ++ [(f, Nothing, [inicial afn1]) | f <- final afn1],
                                inicial = inicial afn1,
                                final = [inicial afn1] ++ final afn1}
        where
            afn1 = reToAFNEpAux m n