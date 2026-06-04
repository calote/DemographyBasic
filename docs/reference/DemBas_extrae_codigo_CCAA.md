# Extrae el código numérico de comunidad autónoma de una columna de texto

Obtiene los dos dígitos iniciales de código de comunidad autónoma de los
nombres de territorios que aparecen en los datos del INE.

## Usage

``` r
DemBas_extrae_codigo_CCAA(columna)
```

## Arguments

- columna:

  Vector de tipo character con los nombres que incluyen el código de
  comunidad autónoma al inicio (ej. "01 Andalucía", "02 Aragón").

## Value

Vector de tipo character con los códigos de comunidad autónoma de dos
dígitos (ej. "01", "02", "03").

## Examples

``` r
nombres_con_codigo <- c("01 Andalucía", "02 Aragón", "03 Asturias")
codigos <- DemBas_extrae_codigo_CCAA(nombres_con_codigo)
codigos
#> [1] "01" "02" "03"
```
