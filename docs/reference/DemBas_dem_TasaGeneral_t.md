# Calcula la tasa general de un evento demográfico

Calcula la tasa general de mortalidad u otro evento demográfico por cada
1000 habitantes de la población de referencia. A diferencia de la tasa
bruta, la tasa general se calcula sobre una población específica (ej.
población en edad fértil para tasas de natalidad).

## Usage

``` r
DemBas_dem_TasaGeneral_t(NumEventos_t, PobMedia_Referida_t)
```

## Arguments

- NumEventos_t:

  Valor numérico o vector con el número de eventos demográficos en el
  período t.

- PobMedia_Referida_t:

  Valor numérico o vector con la población media de referencia para el
  cálculo (ej. mujeres de 15 a 49 años).

## Value

Vector numérico con las tasas generales por 1000 habitantes.

## Examples

``` r
# Tasa general de natalidad
DemBas_dem_TasaGeneral_t(NumEventos_t = 350000, PobMedia_Referida_t = 13000000)
#> [1] 26.92308
```
