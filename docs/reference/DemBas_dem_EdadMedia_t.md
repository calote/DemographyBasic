# Calcula la edad media de un evento demográfico

Calcula la edad media al ocurrencia de un evento demográfico (ej. edad
media de la madre al nacimiento de sus hijos, edad media al matrimonio,
etc.). Se calcula como la media ponderada de las marcas de clase de edad
por las tasas específicas.

## Usage

``` r
DemBas_dem_EdadMedia_t(vTasasEspecificas_x_t, Edad.marcas)
```

## Arguments

- vTasasEspecificas_x_t:

  Vector numérico con las tasas específicas del evento por grupo de
  edad.

- Edad.marcas:

  Vector numérico con las marcas de clase de cada grupo de edad (ej. 2.5
  para el grupo 0-4, 7 para el 5-9, etc.). Debe tener la misma longitud
  que vTasasEspecificas_x_t.

## Value

Valor numérico con la edad media del evento.

## Examples

``` r
# Edad media de la madre al nacimiento
tasas <- c(0.001, 0.005, 0.015, 0.025, 0.035, 0.040, 0.030, 0.015, 0.005)
marcas <- c(2.5, 7, 12, 17, 22, 27, 32, 37, 42)
edad_media <- DemBas_dem_EdadMedia_t(tasas, marcas)
edad_media
#> [1] 0.02466374
```
