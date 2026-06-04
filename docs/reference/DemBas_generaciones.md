# Calcula los años de nacimiento (generaciones) a partir de grupos de edad

Toma un grupo de edad (ej. "0-4", "5-9", "100+") y calcula el rango de
años de nacimiento (generaciones) correspondiente, utilizando un año de
referencia. Por ejemplo, si Año_ref=2020 y x="5-9", devuelve
"2010-2015".

## Usage

``` r
DemBas_generaciones(x, Ano_ref = 2020)
```

## Arguments

- x:

  Cadena de texto o factor con el grupo de edad (ej. "0-4", "5-9",
  "10-14", "100+").

- Ano_ref:

  Valor numérico con el año de referencia para calcular las
  generaciones. Por defecto 2020.

## Value

Cadena de texto con el rango de años de nacimiento (ej. "2010-2015",
"1915-"). Para grupos abiertos como "100+", devuelve algo como "1915-".

## Examples

``` r
DemBas_generaciones("5-9", 2020)
#> [1] "2011-2015"
DemBas_generaciones("100+", 2020)
#> [1] "1920-"
```
