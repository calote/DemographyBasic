# Calcula la Tasa Global de Fecundidad (TFR)

Calcula la Tasa Global de Fecundidad (TFR)

## Usage

``` r
DemBas_tfr(datos, intervalo_edad = 5)
```

## Arguments

- datos:

  Data frame con columnas: edad_mujer, nacimientos, pob_mujeres

- intervalo_edad:

  Amplitud del intervalo de edad (ej: 5 años)

## Value

Lista con: - tfr: Tasa Global de Fecundidad - asfr: Data frame con tasas
específicas de fecundidad

## Details

TFR = Σ (nacimientos_x / pob_mujeres_x) \* intervalo_edad También
retorna tasas específicas de fecundidad (ASFR)
