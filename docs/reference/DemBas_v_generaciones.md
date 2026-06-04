# Calcula las generaciones para un vector de grupos de edad

Versión vectorizada de
[`DemBas_generaciones`](https://DemographyBasic.github.io/reference/DemBas_generaciones.md)
que aplica el cálculo a todos los elementos de un vector. Útil para
añadir etiquetas de generaciones a los ejes de pirámides poblacionales.

## Usage

``` r
DemBas_v_generaciones(x, Ano_ref = 2020)
```

## Arguments

- x:

  Vector de cadenas de texto o factores con los grupos de edad (ej.
  c("0-4", "5-9", "10-14", "100+")).

- Ano_ref:

  Valor numérico con el año de referencia para calcular las
  generaciones. Por defecto 2020.

## Value

Vector de cadenas de texto con los rangos de años de nacimiento para
cada grupo de edad.

## See also

[`DemBas_generaciones`](https://DemographyBasic.github.io/reference/DemBas_generaciones.md)
para la función que procesa un solo valor.

## Examples

``` r
DemBas_v_generaciones(c("0-4", "5-9", "10-14", "100+"), 2020)
#>         0-4         5-9       10-14        100+ 
#> "2016-2020" "2011-2015" "2006-2010"     "1920-" 
```
