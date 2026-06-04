# Calcula la tabla de vida abreviada para edades agrupadas

Construye una tabla de vida abreviada a partir de las tasas de
mortalidad para edades agrupadas (0, 1-4, 5-9, 10-14, ...). El método
aplica coeficientes de separación de Greville para el cálculo de las
personas-año vividas (nLx).

## Usage

``` r
DemBas_tablavida_abreviada2(mx)
```

## Arguments

- mx:

  Vector numérico con las tasas centralizadas de mortalidad para cada
  grupo de edad. Los grupos deben corresponderse con: 0, 1-4, 5-9,
  10-14, ..., 95-99, 100+.

## Value

Data frame con la tabla de vida abreviada que contiene las columnas:

- x:

  Edad inicial de cada grupo (0, 1, 5, 10, ..., 100)

- mx:

  Tasa centralizada de mortalidad (redondeada a 5 decimales)

- qx:

  Probabilidad de muerte en el intervalo n

- px:

  Probabilidad de sobrevivir en el intervalo n

- lx:

  Supervivientes al inicio del intervalo

- dx:

  Defunciones en el intervalo

- Lx:

  Personas-año vividas en el intervalo

- Tx:

  Personas-año restantes desde el inicio del intervalo

- ex:

  Esperanza de vida a la edad x

- Sx:

  Fracción de supervivencia del intervalo

## References

Greville, T. N. E. (1948). Mortality tables analyzed by cause of death.
The American Statistician, 2(4), 23-27.

## Examples

``` r
# Tasas de mortalidad para España 2003
mx0 <- 1733 / 441881
mx <- c(mx0, 0.00027, 0.00013, 0.00016, 0.00043, 0.00057, 0.00059,
        0.00081, 0.00115, 0.00174, 0.00258, 0.00376, 0.00569, 0.00818,
        0.01346, 0.02206, 0.03844, 0.06981, 0.12872, 0.21674, 0.31705,
        0.48258)
tv <- DemBas_tablavida_abreviada2(mx)
tv
#>      x      mx      qx      dx     lx    px     Lx      Tx    ex      Sx
#> 1    0 0.00392 0.00392 0.99608 100000   392  99647 7958969 79.59 0.99564
#> 2    1 0.00027 0.00108 0.99892  99608   108 398173 7859322 78.90 0.99904
#> 3    5 0.00013 0.00065 0.99935  99500    65 497340 7461149 74.99 0.99928
#> 4   10 0.00016 0.00080 0.99920  99436    80 496979 6963809 70.03 0.99853
#> 5   15 0.00043 0.00215 0.99785  99356   213 496247 6466829 65.09 0.99750
#> 6   20 0.00057 0.00285 0.99715  99143   282 495008 5970582 60.22 0.99710
#> 7   25 0.00059 0.00295 0.99705  98861   291 493575 5475574 55.39 0.99651
#> 8   30 0.00081 0.00404 0.99596  98569   398 491851 4981999 50.54 0.99511
#> 9   35 0.00115 0.00573 0.99427  98171   563 489448 4490148 45.74 0.99281
#> 10  40 0.00174 0.00866 0.99134  97608   846 485927 4000700 40.99 0.98927
#> 11  45 0.00258 0.01282 0.98718  96763  1240 480712 3514773 36.32 0.98430
#> 12  50 0.00376 0.01862 0.98138  95522  1779 473164 3034061 31.76 0.97671
#> 13  55 0.00569 0.02805 0.97195  93743  2630 462142 2560897 27.32 0.96602
#> 14  60 0.00818 0.04008 0.95992  91114  3652 446439 2098754 23.03 0.94766
#> 15  65 0.01346 0.06511 0.93489  87462  5695 423073 1652316 18.89 0.91584
#> 16  70 0.02206 0.10453 0.89547  81767  8548 387467 1229243 15.03 0.86201
#> 17  75 0.03844 0.17535 0.82465  73220 12839 334001  841776 11.50 0.76959
#> 18  80 0.06981 0.29718 0.70282  60381 17944 257043  507775  8.41 0.62451
#> 19  85 0.12872 0.48691 0.51309  42437 20663 160526  250731  5.91 0.43986
#> 20  90 0.21674 0.70286 0.29714  21774 15304  70609   90206  4.14 0.25557
#> 21  95 0.31705 0.88432 0.11568   6470  5721  18046   19597  3.03 0.07914
#> 22 100 0.48258 1.00000 0.00000    748   748   1551    1551  2.07      NA
```
