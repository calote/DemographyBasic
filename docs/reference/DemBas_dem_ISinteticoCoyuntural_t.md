# Calcula el Indicador Sintético de Fecundidad (ISF) coyuntural

Calcula el ISF también conocido como Tasa de Fecundidad Total o número
medio de hijos por mujer. Es la suma de las tasas específicas de
fecundidad por edad (mide la fecundidad hipotética de una cohorte si
experimentara las tasas observadas en el período).

## Usage

``` r
DemBas_dem_ISinteticoCoyuntural_t(vTasasEspecificas_x_t, amplitud_intEdad = 1)
```

## Arguments

- vTasasEspecificas_x_t:

  Vector numérico con las tasas específicas por grupo de edad. Deben
  estar expresadas en tasas por 1 (no por 1000).

- amplitud_intEdad:

  Valor numérico con la amplitud del intervalo de grupos de Edad (por
  defecto, es 1 para edades simples, pero si las tasas son para grupos
  quinquenales, se debe usar 5).

## Value

Valor numérico con el número medio de sucesos por individuo.

## Examples

``` r
# ISF a partir de tasas específicas de fecundidad
tasas_fecundidad <- c(0.001, 0.005, 0.015, 0.025, 0.035, 0.040,
                      0.030, 0.015, 0.005, 0.001)
isf <- DemBas_dem_ISinteticoCoyuntural_t(tasas_fecundidad, amplitud_intEdad = 5)
isf
#> [1] 0.86
```
