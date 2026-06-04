# Redondea un vector numérico con corrección de error de punto flotante

Redondea los valores de un vector numérico al número especificado de
decimales. Añade una pequeña corrección para evitar problemas de
representación de punto flotante que pueden causar redondeos incorrectos
(ej. 2.5 redondeado a 2 en lugar de 3).

## Usage

``` r
DemBas_redondear(x, digitos = 0)
```

## Arguments

- x:

  Vector numérico con los valores a redondear.

- digitos:

  Valor entero con el número de decimales a mantener. Si es 0, redondea
  al entero más cercano. Por defecto 0.

## Value

Vector numérico con los valores redondeados.

## Details

El ajuste `sign(x) * 1e-10` compensa el error de representación en
números de punto flotante que puede hacer que valores como 2.9999999 no
se redondeen correctamente.

## Examples

``` r
v1 <- DemBas_redondear(rnorm(20), 2)
v1
#>  [1] -1.40  0.26 -2.44 -0.01  0.62  1.15 -1.82 -0.25 -0.24 -0.28 -0.55  0.63
#> [13]  2.07 -1.63  0.51 -1.86 -0.52 -0.05  0.54 -0.91
```
