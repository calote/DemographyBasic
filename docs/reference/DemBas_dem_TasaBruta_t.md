# Calcula la tasa bruta de un evento demográfico

Calcula la tasa bruta de mortalidad, natalidad u otro evento demográfico
por cada 1000 habitantes. La tasa bruta es el cociente entre el número
de eventos en un período y la población media de ese período,
multiplicado por 1000.

## Usage

``` r
DemBas_dem_TasaBruta_t(NumEventos_t, PobMedia_t)
```

## Arguments

- NumEventos_t:

  Valor numérico o vector con el número de eventos demográficos
  (defunciones, nacimientos, etc.) en el período t.

- PobMedia_t:

  Valor numérico o vector con la población media durante el período t.

## Value

Vector numérico con las tasas brutas por 1000 habitantes.

## Examples

``` r
# Tasa bruta de mortalidad de España en 2020
DemBas_dem_TasaBruta_t(NumEventos_t = 492930, PobMedia_t = 47415750)
#> [1] 10.39591
```
