# Calcula la tabla de vida completa con redondeo manual

Versión de
[`DemBas_tablavida_completa`](https://DemographyBasic.github.io/reference/DemBas_tablavida_completa.md)
que simula el proceso de redondeo manual típico de los cálculos
demográficos tradicionales. Redondea cada columna de forma independiente
en cada paso, imitando el proceso que se realizaría con una calculadora.

## Usage

``` r
DemBas_tablavida_completa_calculadora(Mx, l0 = 1e+05, muestraSx = TRUE)
```

## Arguments

- Mx:

  Vector numérico con las tasas centralizadas de mortalidad para cada
  edad simple.

- l0:

  Valor numérico con la población inicial (radix). Por defecto 100000.

- muestraSx:

  Valor lógico. Si TRUE, incluye la columna Sx en el resultado.

## Value

tibble con la tabla de vida calculada con redondeo manual.

## See also

[`DemBas_tablavida_completa`](https://DemographyBasic.github.io/reference/DemBas_tablavida_completa.md)
para la versión sin redondeo manual.

## Examples

``` r
Mx1000 <- c(9.12160, 0.84807, 0.49502, 0.33352, 0.27296,
            0.23258, 0.20229, 0.19221, 0.19225, 0.18219,
            0.18219, 0.18223, 0.19239, 0.21268, 0.25325,
            0.31411, 0.38518, 0.44618, 0.47682, 0.48721,
            0.48744, 0.48768, 0.48792, 0.48816, 0.48840,
            0.48864, 0.49907, 0.49932, 0.49957, 0.51002,
            0.52049, 0.55140, 0.58236, 0.62360, 0.67515,
            0.72681, 0.79907, 0.89203, 0.99549, 1.09927,
            1.22398, 1.35944, 1.50578, 1.68380, 1.87305,
            2.07374, 2.28609, 2.52075, 2.76762, 3.04801,
            3.34149, 3.64844, 3.99052, 4.35799, 4.76231,
            5.19386, 5.68611, 6.20842, 6.79514, 7.42672,
            8.12774, 8.89053, 9.74129, 10.68500, 11.73947,
            12.91226, 14.22468, 15.71228, 17.35403, 19.16595,
            21.20612, 23.43628, 25.96366, 28.83038, 32.10259,
            35.83456, 40.09691, 44.96477, 50.47392, 56.71130,
            63.73696, 71.61161, 80.38833, 90.15169, 100.87032,
            112.56462, 125.25733, 138.92967, 153.57492, 169.22923,
            185.87183, 203.41806, 222.05303, 241.69867, 262.24030,
            283.83279, 306.41026, 329.80973, 354.16667, 379.65616,
            406.15058, 434.57189, 462.12121, 491.86992, 832.50000)
mx <- Mx1000 / 1000
tb01 <- DemBas_tablavida_completa_calculadora(mx)
head(tb01, 10)
#> # A tibble: 10 × 10
#>    Edad  Mx1000      qx    px     lx    dx    Lx      Tx    ex    Sx
#>    <chr>  <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl>
#>  1 0       9.12 0.00908 0.991 100000   908 99364 7826362  78.3 0.997
#>  2 1       0.85 0.00085 0.999  99092    84 99042 7726998  78.0 0.999
#>  3 2       0.5  0.0005  1.000  99008    50 98983 7627956  77.0 1.000
#>  4 3       0.33 0.00033 1.000  98958    33 98942 7528973  76.1 1.000
#>  5 4       0.27 0.00027 1.000  98926    27 98913 7430031  75.1 1.000
#>  6 5       0.23 0.00023 1.000  98899    23 98888 7331118  74.1 1.000
#>  7 6       0.2  0.0002  1.000  98876    20 98866 7232230  73.1 1.000
#>  8 7       0.19 0.00019 1.000  98856    19 98847 7133364  72.2 1.000
#>  9 8       0.19 0.00019 1.000  98838    19 98829 7034517  71.2 1.000
#> 10 9       0.18 0.00018 1.000  98819    18 98810 6935688  70.2 1.000
```
