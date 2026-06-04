# Calcula la tabla de vida completa (Rowland)

Construye una tabla de vida completa a partir de las tasas de mortalidad
para edades simples (0, 1, 2, ...). Utiliza el método de Chiang para qx
y coeficientes de Greville para Lx.

## Usage

``` r
DemBas_tablavida_completa(Mx, l0 = 1e+05, redondeo = TRUE, muestraSx = TRUE)
```

## Arguments

- Mx:

  Vector numérico con las tasas centralizadas de mortalidad para cada
  edad simple. Valores menores que 1 (tasas por persona).

- l0:

  Valor numérico con la población inicial (radix). Por defecto 100000.

- redondeo:

  Valor lógico. Si TRUE, redondea los resultados según convenciones
  demográficas estándar.

- muestraSx:

  Valor lógico. Si TRUE, incluye la columna Sx en el resultado.

## Value

tibble con la tabla de vida que contiene las columnas:

- Edad:

  Edad simple (0, 1, 2, ..., n+)

- Mx1000:

  Tasa centralizada de mortalidad por 1000

- qx:

  Probabilidad de muerte entre edad x y x+1

- px:

  Probabilidad de sobrevivir de edad x a x+1

- lx:

  Supervivientes a la edad x

- dx:

  Defunciones en el intervalo \[x, x+1)

- Lx:

  Personas-año vividas en el intervalo \[x, x+1)

- Tx:

  Personas-año restantes desde edad x

- ex:

  Esperanza de vida a la edad x

- Sx:

  Fracción de supervivencia (solo si muestraSx=TRUE)

## References

Rowland, D. T. (2003). Demographic Techniques. Sydney: Pergamon Press.

Chiang, C. L. (1984). The Life Table and Its Applications. Malabar, FL:
Robert E. Krieger Publishing Company.

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
tb01 <- DemBas_tablavida_completa(mx)
head(tb01, 10)
#> # A tibble: 10 × 10
#>    Edad  Mx1000      qx    px     lx    dx    Lx      Tx    ex    Sx
#>    <chr>  <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl>
#>  1 0      9.12  0.00908 0.991 100000   908 99364 7826402  78.3 0.997
#>  2 1      0.848 0.00085 0.999  99092    84 99042 7727038  78.0 0.999
#>  3 2      0.495 0.00049 1.000  99008    49 98983 7627996  77.0 1.000
#>  4 3      0.334 0.00033 1.000  98959    33 98942 7529013  76.1 1.000
#>  5 4      0.273 0.00027 1.000  98926    27 98912 7430070  75.1 1.000
#>  6 5      0.233 0.00023 1.000  98899    23 98887 7331158  74.1 1.000
#>  7 6      0.202 0.0002  1.000  98876    20 98866 7232270  73.1 1.000
#>  8 7      0.192 0.00019 1.000  98856    19 98846 7133404  72.2 1.000
#>  9 8      0.192 0.00019 1.000  98837    19 98827 7034558  71.2 1.000
#> 10 9      0.182 0.00018 1.000  98818    18 98809 6935730  70.2 1.000
```
