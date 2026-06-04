# Calcula indicadores estructurales de una población

Calcula indicadores estructurales de una población

## Usage

``` r
DemBas_indicadores_estructura(datos, grupos_edad = c(0, 15, 65, Inf))
```

## Arguments

- datos:

  Data frame con columnas: edad, sexo, poblacion

- grupos_edad:

  Vector con límites de grupos de edad (ej: c(0,15,65,Inf))

## Value

Data frame con indicadores

## Details

Calcula: - Ratio de dependencia (jóvenes y ancianos) - Índice de
envejecimiento - Razón de masculinidad -
