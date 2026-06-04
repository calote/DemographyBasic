# Agrupa datos etarios en intervalos

Toma datos con edades individuales (o ya agrupados) y los reagrupa en
los intervalos de edad especificados, sumando las poblaciones de hombres
y mujeres dentro de cada grupo y calculando la amplitud correspondiente.

## Usage

``` r
agrupar_edad(datos, intervalos)
```

## Arguments

- datos:

  Data frame con columnas `edad_grupo`, `hombre`, `mujer` y `amplitud`.

- intervalos:

  Vector de caracteres con las etiquetas de los intervalos destino (ej.
  `c("0-4", "5-9", ...)`). También acepta la cadena `"estandar1"` para
  usar los intervalos quinquenales estándar predefinidos.

## Value

Data frame con columnas `edad_grupo`, `hombre`, `mujer` y `amplitud`,
ordenado según `intervalos`.
