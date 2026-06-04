# Elimina el código de comunidad autónoma de una columna de texto

Elimina los dos dígitos iniciales de código de comunidad autónoma (ej.
"01", "02", ..., "19") de los nombres de territorios que aparecen en los
datos del INE. Este código va seguido de un espacio y el nombre completo
de la comunidad.

## Usage

``` r
DemBas_elimina_codigo_CCAA(columna)
```

## Arguments

- columna:

  Vector de tipo character con los nombres que incluyen el código de
  comunidad autónoma al inicio (ej. "01 Andalucía", "02 Aragón").

## Value

Vector de tipo character con los nombres sin el código inicial y sin
espacios adicionales (ej. "Andalucía", "Aragón").

## Examples

``` r
nombres_con_codigo <- c("01 Andalucía", "02 Aragón", "03 Asturias")
nombres_limpios <- DemBas_elimina_codigo_CCAA(nombres_con_codigo)
nombres_limpios
#> [1] "Andalucía" "Aragón"    "Asturias" 
```
