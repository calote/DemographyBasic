# Función "todo en uno": calcula indicadores demográficos completos

Función "todo en uno": calcula indicadores demográficos completos

## Usage

``` r
DemBas_indicadores_demograficos(datos, nacimientos = NA, l0 = 1e+05)
```

## Arguments

- datos:

  Data frame con columnas: edad, sexo, poblacion, defunciones (opcional)

- nacimientos:

  Número anual de nacimientos

- l0:

  Raíz de la tabla de vida (defecto 100000)

## Value

Data frame resumen

## Details

Retorna un resumen con: - Tasas brutas (natalidad, mortalidad) -
Crecimiento natural - Indicadores estructurales - Esperanza de vida (si
hay defunciones)
