# Calcula la tabla de vida abreviada con redondeo manual

Versión de
[`DemBas_tablavida_abreviada`](https://DemographyBasic.github.io/reference/DemBas_tablavida_abreviada.md)
que simula el proceso de redondeo manual típico de los cálculos
demográficos tradicionales. Redondea cada columna de forma independiente
en cada paso, imitando el proceso que se realizaría con una calculadora.

## Usage

``` r
DemBas_tablavida_abreviada_calculadora(nMx, l0 = 1e+05, muestraSx = TRUE)
```

## Arguments

- nMx:

  Vector numérico con las tasas centralizadas de mortalidad para cada
  grupo de edad quinquenal.

- l0:

  Valor numérico con la población inicial (radix). Por defecto 100000.

- muestraSx:

  Valor lógico. Si TRUE, incluye la columna Sx en el resultado.

## Value

tibble con la tabla de vida abreviada calculada con redondeo manual.

## See also

[`DemBas_tablavida_abreviada`](https://DemographyBasic.github.io/reference/DemBas_tablavida_abreviada.md)
para la versión sin redondeo manual.

## Examples

``` r
mx0 <- 1733 / 441881
mx <- c(mx0, 0.00027, 0.00013, 0.00016, 0.00043, 0.00057, 0.00059,
        0.00081, 0.00115, 0.00174, 0.00258, 0.00376, 0.00569, 0.00818,
        0.01346, 0.02206, 0.03844, 0.06981, 0.12872, 0.21674, 0.31705,
        0.48258)
tv <- DemBas_tablavida_abreviada_calculadora(mx)
tv
#> # A tibble: 22 × 11
#>    Edad      n nMx1000     nqx   npx     lx   ndx    nLx      Tx    ex    Sx
#>    <chr> <dbl>   <dbl>   <dbl> <dbl>  <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl>
#>  1 0         1    3.92 0.00391 0.996 100000   391  99726 7959163  79.6 0.996
#>  2 1         4    0.27 0.00108 0.999  99609   108 398220 7859437  78.9 0.999
#>  3 5         5    0.13 0.00065 0.999  99501    65 497345 7461217  75.0 0.999
#>  4 10        5    0.16 0.0008  0.999  99437    80 496985 6963872  70.0 0.999
#>  5 15        5    0.43 0.00215 0.998  99357   214 496253 6466887  65.1 0.998
#>  6 20        5    0.57 0.00285 0.997  99144   283 495013 5970634  60.2 0.997
#>  7 25        5    0.59 0.00295 0.997  98861   292 493575 5475621  55.4 0.997
#>  8 30        5    0.81 0.00404 0.996  98569   398 491850 4982046  50.5 0.995
#>  9 35        5    1.15 0.00573 0.994  98171   563 489450 4490196  45.7 0.993
#> 10 40        5    1.74 0.00866 0.991  97609   845 485930 4000746  41.0 0.989
#> # ℹ 12 more rows
```
