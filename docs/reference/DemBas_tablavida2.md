# Calcula la tabla de vida para edades simples o completa

Construye una tabla de vida a partir de las tasas de mortalidad para
edades simples (0, 1, 2, ...). Utiliza el método de Chiang para el
cálculo de qx y coeficientes de Greville para las personas-año vividas
(Lx).

## Usage

``` r
DemBas_tablavida2(mx)
```

## Arguments

- mx:

  Vector numérico con las tasas centralizadas de mortalidad para cada
  edad simple (desde 0 hasta la edad máxima observada). Los valores
  deben ser tasas (no probabilidades), expresadas por persona (valores
  entre 0 y 1).

## Value

Data frame con la tabla de vida que contiene las siguientes columnas:

- x:

  Edad simple (0, 1, 2, ..., n)

- mx:

  Tasa centralizada de mortalidad (redondeada a 5 decimales)

- qx:

  Probabilidad de muerte entre edad x y x+1

- px:

  Probabilidad de sobrevivir de edad x a x+1

- lx:

  Supervivientes a la edad x (radix = 100000)

- dx:

  Defunciones en el intervalo \[x, x+1)

- Lx:

  Personas-año vividas en el intervalo \[x, x+1)

- Tx:

  Personas-año restantes desde edad x

- ex:

  Esperanza de vida a la edad x

- Sx:

  Fracción de supervivencia (proporción que pasa de x a x+1)

## References

Chiang, C. L. (1984). The Life Table and Its Applications. Malabar, FL:
Robert E. Krieger Publishing Company.

Greville, T. N. E. (1948). Mortality tables analyzed by cause of death.
The American Statistician, 2(4), 23-27.

## Examples

``` r
# Ejemplo con tasas de mortalidad por 1000 (convertir a tasas por persona)
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
tb01 <- DemBas_tablavida2(mx)
head(tb01, 10)
#>    x      mx      qx      dx     lx  px    Lx      Tx    ex      Sx
#> 1  0 0.00912 0.00912 0.99088 100000 912 99179 7825883 78.26 0.99857
#> 2  1 0.00085 0.00085 0.99915  99088  84 99037 7726704 77.98 0.99936
#> 3  2 0.00050 0.00049 0.99951  99004  49 98974 7627667 77.04 0.99960
#> 4  3 0.00033 0.00033 0.99967  98955  33 98935 7528692 76.08 0.99970
#> 5  4 0.00027 0.00027 0.99973  98922  27 98906 7429757 75.11 0.99977
#> 6  5 0.00023 0.00023 0.99977  98895  23 98883 7330851 74.13 0.99978
#> 7  6 0.00020 0.00020 0.99980  98872  20 98862 7231968 73.14 0.99980
#> 8  7 0.00019 0.00019 0.99981  98852  19 98842 7133106 72.16 0.99981
#> 9  8 0.00019 0.00019 0.99981  98833  19 98823 7034264 71.17 0.99981
#> 10 9 0.00018 0.00018 0.99982  98814  18 98805 6935441 70.19 0.99982
```
