# Calcula las tasas centralizadas de mortalidad para edades agrupadas

Calcula las tasas centralizadas de mortalidad (nMx) para grupos de edad
quinquenales a partir de defunciones y población media, incluyendo el
tratamiento especial para el grupo de menores de un año (TMI).

## Usage

``` r
DemBas_mx_abreviada(Px, Dx, N0, D0)
```

## Arguments

- Px:

  Vector numérico con la población media por grupo de edad quinquenal.
  El primer elemento corresponde a edad 0, el segundo a 1-4, y así
  sucesivamente hasta 100+.

- Dx:

  Vector numérico con las defunciones por grupo de edad quinquenal, con
  el mismo formato que Px.

- N0:

  Número de nacimientos en el periodo (usados para calcular la TMI).

- D0:

  Número de defunciones de menores de un año en el periodo.

## Value

Vector numérico con las tasas centralizadas de mortalidad (nMx) para
cada grupo de edad. El primer elemento corresponde a la tasa de
mortalidad infantil (D0/N0) y los restantes son Dx/Px.

## Examples

``` r
# Datos de España 2003
Px <- c(NA, 1627456, 1938350, 2104636, 2388049, 3070467, 3614444, 3545550,
        3431304, 3182840, 2791972, 2498361, 2334676, 1953022, 1978465,
        1898370, 1492487, 974162, 495260, 203924, 46078, 5139)
Dx <- c(NA, 442, 254, 341, 1032, 1746, 2136, 2872,
        2933, 5545, 7193, 9401, 13294, 15972, 26636,
        41879, 57377, 68007, 63007, 44198, 14609, 2480)
N0 <- 441881
D0 <- 1733
mx <- DemBas_mx_abreviada(Px, Dx, N0, D0)
mx
#>  [1] 0.00392 0.00027 0.00013 0.00016 0.00043 0.00057 0.00059 0.00081 0.00085
#> [10] 0.00174 0.00258 0.00376 0.00569 0.00818 0.01346 0.02206 0.03844 0.06981
#> [19] 0.12722 0.21674 0.31705 0.48258
```
