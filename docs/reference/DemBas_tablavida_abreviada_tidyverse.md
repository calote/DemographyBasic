# Calcula la tabla de vida abreviada (método tidyverse)

Versión de
[`DemBas_tablavida_abreviada`](https://DemographyBasic.github.io/reference/DemBas_tablavida_abreviada.md)
que replica el estilo de cálculo con `tidyverse` usado en prácticas
docentes. Utiliza `cumprod(lag(npx))` para `lx` y ajusta `nqx` según la
amplitud de cada grupo de edad.

## Usage

``` r
DemBas_tablavida_abreviada_tidyverse(nMx, l0 = 1e+05, redondeo = TRUE)
```

## Arguments

- nMx:

  Vector numérico con las tasas centralizadas de mortalidad para cada
  grupo de edad (0, 1-4, 5-9, 10-14, ..., 100+).

- l0:

  Valor numérico con la población inicial (radix). Por defecto 100000.

- redondeo:

  Lógico. Si `TRUE` (por defecto), aplica `DemBas_redondear` a los
  resultados: nMx (5 dígitos), nqx (5), npx (5), lx (0), ndx (0), nLx
  (0), Tx (0), ex (2). Si `FALSE`, retorna valores sin redondear para
  ver precisión completa.

## Value

tibble con columnas: Edad, nMx, nqx, npx, lx, ndx, nLx, Tx, ex.

## Details

La función opera en dos fases:

1.  **Cálculo**: Todos los valores se calculan sin redondeo intermedio,
    permitiendo precisión máxima en los cálculos intermedios.

2.  **Presentación**: Si `redondeo = TRUE` (por defecto), se aplica
    [`DemBas_redondear`](https://DemographyBasic.github.io/reference/DemBas_redondear.md)
    (redondeo con epsilon) a cada columna. Si `redondeo = FALSE`, se
    retornan los valores sin redondear.

## Examples

``` r
if (FALSE) { # \dontrun{
mx0 <- 1733 / 441881
mx <- c(mx0, 0.00027, 0.00013, 0.00016, 0.00043, 0.00057, 0.00059,
        0.00081, 0.00115, 0.00174, 0.00258, 0.00376, 0.00569, 0.00818,
        0.01346, 0.02206, 0.03844, 0.06981, 0.12872, 0.21674, 0.31705,
        0.48258)
tv <- DemBas_tablavida_abreviada_tidyverse(mx)
tv

# Ver valores sin redondear
tv_sin_redondeo <- DemBas_tablavida_abreviada_tidyverse(mx, redondeo = FALSE)
tv_sin_redondeo
} # }
```
