# Crea múltiples pirámides poblacionales enfrentadas en una cuadrícula

Genera varias pirámides poblacionales organizadas en una cuadrícula de
facetas, permitiendo comparar pirámides de diferentes categorías o
períodos. Utiliza internamente
[`DemBas_piramide_ggplot2`](https://DemographyBasic.github.io/reference/DemBas_piramide_ggplot2.md).

## Usage

``` r
DemBas_piramides_enfrentadas_ggplot2(
  datosPiramide,
  porcentajes = TRUE,
  etiquetas = FALSE,
  etiquetas.size = 4,
  UsaCaso = TRUE,
  etiq.hombre = "Hombre",
  etiq.mujer = "Mujer",
  colorear = "Sexo",
  colores = NULL,
  nfilas = NULL,
  ncols = NULL
)
```

## Arguments

- datosPiramide:

  data.frame o tibble con columnas:

  Edad

  :   Variable categórica con edades

  Sexo

  :   Variable categórica ("Hombre" o "Mujer")

  Poblacion

  :   Variable numérica (absolutos)

  Caso

  :   Variable categórica que define los grupos a comparar

- porcentajes:

  Valor lógico. Si TRUE, muestra la población en porcentajes. Por
  defecto TRUE.

- etiquetas:

  Valor lógico. Si TRUE, muestra etiquetas con los valores. Por defecto
  FALSE.

- etiquetas.size:

  Valor numérico para el tamaño del texto de las etiquetas. Por defecto
  4.

- UsaCaso:

  Valor lógico. Si TRUE, calcula porcentajes por cada categoría de la
  variable "Caso". Por defecto TRUE.

- etiq.hombre:

  Cadena de texto con la etiqueta para hombres. Por defecto "Hombre".

- etiq.mujer:

  Cadena de texto con la etiqueta para mujeres. Por defecto "Mujer".

- colorear:

  Cadena de texto que indica la variable para el color de las barras:
  "Sexo", "Edad" o "Poblacion". Por defecto "Sexo".

- colores:

  Vector opcional de colores para las barras.

- nfilas:

  Valor numérico opcional para el número de filas de la cuadrícula de
  facetas.

- ncols:

  Valor numérico opcional para el número de columnas de la cuadrícula de
  facetas.

## Value

Objeto ggplot2 con las pirámides enfrentadas en facetas.

## See also

[`DemBas_piramide_ggplot2`](https://DemographyBasic.github.io/reference/DemBas_piramide_ggplot2.md)
para la función base.
