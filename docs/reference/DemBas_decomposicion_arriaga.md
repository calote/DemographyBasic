# Descomposición Arriaga de la diferencia en esperanza de vida

Descomposición Arriaga de la diferencia en esperanza de vida

## Usage

``` r
DemBas_decomposicion_arriaga(tv1, tv2, grupos_edad = c(0, 1, 5, 15, 45, 65))
```

## Arguments

- tv1:

  Tabla de vida población 1

- tv2:

  Tabla de vida población 2

- grupos_edad:

  Vector con límites de grupos de edad

## Value

Data frame con descomposición

## Details

Descompone la diferencia en esperanza de vida en contribuciones por
grupo de edad. Método Arriaga: separa efecto de mortalidad temprana y
tardía.
