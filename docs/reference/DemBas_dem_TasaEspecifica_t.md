# Calcula tasas específicas de mortalidad (por edad)

Calcula tasas específicas para cada grupo de edad, expresadas por cada
1000 habitantes. Las tasas específicas permiten comparar estructuras
demográficas controlando por la distribución por edades.

## Usage

``` r
DemBas_dem_TasaEspecifica_t(vNumEventos_x_t, vPobMedia_x_t)
```

## Arguments

- vNumEventos_x_t:

  Vector numérico con el número de eventos (ej. defunciones) por grupo
  de edad x en el período t.

- vPobMedia_x_t:

  Vector numérico con la población media por grupo de edad x en el
  período t. Debe tener la misma longitud que vNumEventos_x_t.

## Value

Vector numérico con las tasas específicas por 1000 habitantes para cada
grupo de edad.

## Examples

``` r
# Tasas específicas de mortalidad por edades
eventos <- c(1200, 800, 600, 400, 300, 200, 150, 100, 80, 70)
poblacion <- c(50000, 60000, 70000, 80000, 90000, 95000, 100000, 85000, 70000, 60000)
tasas <- DemBas_dem_TasaEspecifica_t(eventos, poblacion)
tasas
#>  [1] 24.000000 13.333333  8.571429  5.000000  3.333333  2.105263  1.500000
#>  [8]  1.176471  1.142857  1.166667
```
