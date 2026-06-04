# Redondea las columnas numéricas de un data.frame

Aplica redondeo a todas las columnas de tipo numérico en un data.frame,
manteniendo las columnas no numéricas sin cambios. Utiliza la misma
corrección de punto flotante que
[`DemBas_redondear`](https://DemographyBasic.github.io/reference/DemBas_redondear.md).

## Usage

``` r
DemBas_redondear_df(df, digitos = 0)
```

## Arguments

- df:

  data.frame con columnas numéricas y no numéricas.

- digitos:

  Valor entero con el número de decimales a mantener en las columnas
  numéricas. Por defecto 0.

## Value

data.frame con las columnas numéricas redondeadas y las columnas no
numéricas sin cambios.

## Examples

``` r
df <- data.frame(nombres = sample(letters, 20),
                 x = rnorm(20),
                 y = rnorm(20))
df2 <- DemBas_redondear_df(df, 2)
df2
#>    nombres     x     y
#> 1        u -0.59  0.98
#> 2        f -0.33  1.15
#> 3        i -0.09  1.22
#> 4        v -2.05  0.00
#> 5        j  0.15  0.76
#> 6        z -0.29  0.34
#> 7        b  0.25  0.17
#> 8        q -0.55  1.40
#> 9        a  1.41 -0.68
#> 10       y -0.80  0.74
#> 11       t -1.57 -0.86
#> 12       c -1.04  0.42
#> 13       n  1.02  1.45
#> 14       r -0.70  0.19
#> 15       x  0.97 -0.69
#> 16       p -0.08  1.34
#> 17       e  0.89  2.74
#> 18       h -0.78 -0.94
#> 19       m  0.44 -1.78
#> 20       d  0.41 -0.72
```
