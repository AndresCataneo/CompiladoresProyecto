# ProyectoCompiladores

## Integrantes:
- Cataneo Tortolero Andrés Rodrigo
- Frías Domínguez Oscar Fernando 
- Ortiz Cervantes Leonardo Rafael
- Torres Nava Hazel
- Victoria Morales Ricardo Maximiliano

## Requisitos
- Stack
- Haskell (GHC)
- Happy

## Para revisión del proyecto
Para poder compilar y ejecutar nuestro proyecto, realizar los siguientes pasos.

### Compilación
Desde la raíz del proyecto (`CompiladoresProyecto/ProyectoCompiladores`), ejecutar:
```bash
stack build
```

### Ejecución
Desde la raíz del proyecto (`CompiladoresProyecto/ProyectoCompiladores`), ejecutar cualquiera de los comandos siguientes:
#### Comandos tipo 1
Se imprime en terminal la expresión regular, el AFN-épsilon, el AFN, el AFD, el AFD mínimo, la MDD, el código fuente y la tokenización en una columna. Tarda 5 min.
```bash
stack exec ProyectoCompiladores-exe app/IMP.txt app/codigoFuente3.txt
```
```bash
stack exec ProyectoCompiladores-exe app/IMP.txt app/codigoFuente2.txt
```
```bash
stack exec ProyectoCompiladores-exe app/IMP.txt app/codigoFuente1.txt
```
#### Comandos tipo 2
Se imprime en terminal la expresión regular, el AFN-épsilon, el AFN, el AFD, el AFD mínimo y la MDD.
```bash
stack exec ProyectoCompiladores-exe app/IMP.txt
```
