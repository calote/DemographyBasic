# Extrae la edad numérica de una columna de texto con formato

Extrae el número inicial de edad de cadenas de texto que contienen la
edad seguida de texto descriptivo (ej. "25 años", "3 años", "0 meses").
Útil para limpiar datos de edad provenientes de fuentes estadísticas.

## Usage

``` r
DemBas_extrae_Num_Edad(columna)
```

## Arguments

- columna:

  Vector de tipo character con las cadenas de texto que contienen la
  edad al inicio (ej. "25 años", "0 edad").

## Value

Vector de tipo character con los números de edad extraídos (ej. "25",
"0").

## Examples

``` r
textos_edad <- c("25 años", "3 años", "0 edad", "75 años")
edades <- DemBas_extrae_Num_Edad(textos_edad)
edades
#> [1] "25" "3"  "0"  "75"
```
