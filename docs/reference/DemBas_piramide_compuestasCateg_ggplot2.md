# Crea pirámides poblacionales compuestas por categorías

Genera una pirámide de población donde cada barra está subdividida en
segmentos que representan diferentes categorías (ej. españoles/
extranjeros, nacionalidades). A diferencia de las pirámides
superpuestas, aquí los segmentos se apilan dentro de cada barra.

## Usage

``` r
DemBas_piramide_compuestasCateg_ggplot2(
  datosPiramide,
  porcentajes = TRUE,
  etiquetas = FALSE,
  etiquetas.size = 4,
  colores = NULL,
  ordeninverso = FALSE,
  alfa = 1,
  bar.size = 1,
  etiq.hombre = "Hombre",
  etiq.mujer = "Mujer"
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

  :   Variable categórica que identifica cada segmento (ej. grupo de
      nacionalidad)

- porcentajes:

  Valor lógico. Si TRUE, muestra la población en porcentajes. Por
  defecto TRUE.

- etiquetas:

  Valor lógico. Si TRUE, muestra etiquetas con los valores. Por defecto
  FALSE.

- etiquetas.size:

  Valor numérico para el tamaño del texto de las etiquetas. Por defecto
  4.

- colores:

  Vector opcional de colores para cada categoría.

- ordeninverso:

  Valor lógico. Si TRUE, invierte el orden de apilamiento de los
  segmentos. Por defecto FALSE.

- alfa:

  Valor numérico entre 0 y 1 para la transparencia de las barras. Por
  defecto 1 (sin transparencia).

- bar.size:

  Valor numérico para el grosor del borde de las barras. Por defecto 1.

- etiq.hombre:

  Cadena de texto con la etiqueta para hombres. Por defecto "Hombre".

- etiq.mujer:

  Cadena de texto con la etiqueta para mujeres. Por defecto "Mujer".

## Value

Objeto ggplot2 con la pirámide compuesta.
