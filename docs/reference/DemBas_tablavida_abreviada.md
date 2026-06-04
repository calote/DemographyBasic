# Calcula la tabla de vida abreviada (Rowland)

Construye una tabla de vida abreviada a partir de las tasas de
mortalidad para grupos de edad quinquenales. El método aplica
coeficientes de separación de Greville para el cálculo de las
personas-año vividas (nLx).

## Usage

``` r
DemBas_tablavida_abreviada(nMx, l0 = 1e+05, redondeo = TRUE, muestraSx = TRUE)
```

## Arguments

- nMx:

  Vector numérico con las tasas centralizadas de mortalidad para cada
  grupo de edad quinquenal. Los grupos deben corresponderse con: 0, 1-4,
  5-9, 10-14, ..., 95-99, 100+.

- l0:

  Valor numérico con la población inicial (radix). Por defecto 100000.

- redondeo:

  Valor lógico. Si TRUE, redondea los resultados según convenciones
  demográficas estándar.

- muestraSx:

  Valor lógico. Si TRUE, incluye la columna Sx en el resultado.

## Value

tibble con la tabla de vida abreviada que contiene las columnas:

- Edad:

  Edad inicial de cada grupo (0, 1, 5, 10, ..., 100+)

- n:

  Longitud del intervalo de edad

- nMx1000:

  Tasa centralizada de mortalidad por 1000

- nqx:

  Probabilidad de muerte en el intervalo n

- npx:

  Probabilidad de sobrevivir en el intervalo n

- lx:

  Supervivientes al inicio del intervalo

- ndx:

  Defunciones en el intervalo

- nLx:

  Personas-año vividas en el intervalo

- Tx:

  Personas-año restantes desde el inicio del intervalo

- ex:

  Esperanza de vida a la edad x

- Sx:

  Fracción de supervivencia del intervalo

## References

Rowland, D. T. (2003). Demographic Techniques. Sydney: Pergamon Press.

Greville, T. N. E. (1948). Mortality tables analyzed by cause of death.
The American Statistician, 2(4), 23-27.

## Examples

``` r
mx0 <- 1733 / 441881
mx <- c(mx0, 0.00027, 0.00013, 0.00016, 0.00043, 0.00057, 0.00059,
        0.00081, 0.00115, 0.00174, 0.00258, 0.00376, 0.00569, 0.00818,
        0.01346, 0.02206, 0.03844, 0.06981, 0.12872, 0.21674, 0.31705,
        0.48258)
tv <- DemBas_tablavida_abreviada(mx)
tv
#> # A tibble: 22 × 11
#>    Edad      n nMx1000     nqx   npx     lx   ndx    nLx      Tx    ex    Sx
#>    <chr> <dbl>   <dbl>   <dbl> <dbl>  <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl>
#>  1 0         1    3.92 0.00391 0.996 100000   391  99726 7959151  79.6 0.996
#>  2 1         4    0.27 0.00108 0.999  99609   108 398219 7859425  78.9 0.999
#>  3 5         5    0.13 0.00065 0.999  99501    65 497344 7461205  75.0 0.999
#>  4 10        5    0.16 0.0008  0.999  99436    80 496983 6963862  70.0 0.999
#>  5 15        5    0.43 0.00215 0.998  99357   213 496251 6466879  65.1 0.998
#>  6 20        5    0.57 0.00285 0.997  99144   282 495012 5970628  60.2 0.997
#>  7 25        5    0.59 0.00295 0.997  98861   291 493579 5475615  55.4 0.997
#>  8 30        5    0.81 0.00404 0.996  98570   398 491855 4982037  50.5 0.995
#>  9 35        5    1.15 0.00573 0.994  98172   563 489451 4490182  45.7 0.993
#> 10 40        5    1.74 0.00866 0.991  97609   846 485931 4000731  41.0 0.989
#> # ℹ 12 more rows
```
