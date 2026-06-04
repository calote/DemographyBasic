# Extrae el código numérico de provincia del nombre

Obtiene los dígitos iniciales de código de provincia de los nombres de
provincia que aparecen en los datos del INE. El código va separado del
nombre por un espacio en blanco.

## Usage

``` r
DemBas_extrae_codigo_provincia(vprovincias)
```

## Arguments

- vprovincias:

  Vector de tipo character con los nombres de provincia que incluyen el
  código al inicio (ej. "01 Albacete", "02 Alicante").

## Value

Vector de tipo character con los códigos de provincia extraídos (ej.
"01", "02").

## Examples

``` r
nombres_provincias <- c("01 Albacete", "02 Alicante", "03 Almería")
codigos <- DemBas_extrae_codigo_provincia(nombres_provincias)
codigos
#> 01 Albacete 02 Alicante  03 Almería 
#>        "01"        "02"        "03" 
```
