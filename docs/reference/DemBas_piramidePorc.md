# Crea pirámide poblacional con porcentajes y etiquetas

Genera una pirámide de población donde las barras representan
porcentajes de la población total. Incluye etiquetas con los valores
numéricos en cada barra y opciones para añadir segmentos que separen
grupos de edad (jóvenes, adultos, mayores).

## Usage

``` r
DemBas_piramidePorc(
  datosPiramide,
  Gtitulo = "Pirámide Población",
  Gsubtitulo = "Año 2020",
  Gtitulo.X = "Porcentajes",
  GHombresEtiq = "Hombres",
  GMujeresEtiq = "Mujeres",
  Gedadfinal = 100,
  Gext_izq = -4.5,
  Gext_der = 4.5,
  Glimite = 0.5,
  Gsizeletra = 2.5,
  GSegmentos = TRUE,
  Gguardar = FALSE,
  Garchivo = "piramide.png",
  Gwidth = 12,
  Gheight = 10
)
```

## Arguments

- datosPiramide:

  data.frame o tibble con columnas:

  Edad

  :   Variable categórica o numérica con las edades

  Sexo

  :   Variable categórica ("Hombres" o "Mujeres")

  Poblacion

  :   Variable numérica con datos de población (absolutos)

- Gtitulo:

  Cadena de texto con el título del gráfico. Por defecto "Pirámide
  Población".

- Gsubtitulo:

  Cadena de texto con el subtítulo del gráfico. Por defecto "Año 2020".

- Gtitulo.X:

  Cadena de texto con la etiqueta del eje X. Por defecto "Porcentajes".

- GHombresEtiq:

  Cadena de texto con la etiqueta para hombres. Por defecto "Hombres".

- GMujeresEtiq:

  Cadena de texto con la etiqueta para mujeres. Por defecto "Mujeres".

- Gedadfinal:

  Valor numérico con la edad máxima final para agrupar. Por defecto 100.

- Gext_izq:

  Valor numérico con el límite izquierdo de la escala del eje Y (debe
  ser negativo). Por defecto -4.5.

- Gext_der:

  Valor numérico con el límite derecho de la escala del eje Y (debe ser
  positivo). Por defecto 4.5.

- Glimite:

  Valor numérico con el umbral mínimo para mostrar etiquetas. Por
  defecto 0.5.

- Gsizeletra:

  Valor numérico para el tamaño de la letra de las etiquetas. Por
  defecto 2.5.

- GSegmentos:

  Valor lógico. Si TRUE, añade segmentos verticales para separar grupos
  de edad. Por defecto TRUE.

- Gguardar:

  Valor lógico. Si TRUE, guarda el gráfico en un archivo. Por defecto
  FALSE.

- Garchivo:

  Cadena de texto con el nombre del archivo para guardar. Por defecto
  "piramide.png".

- Gwidth:

  Valor numérico con el ancho de la imagen en pulgadas. Por defecto 12.

- Gheight:

  Valor numérico con el alto de la imagen en pulgadas. Por defecto 10.

## Value

Objeto ggplot2 con la pirámide poblacional.

## Examples

``` r
if (FALSE) { # \dontrun{
load(file = system.file("examples/04003px.RData",
                        package = "DemographyBasic"))

datosPiramide <- datos |>
  dplyr::filter(Ano == 2020,
                Sexo %in% c("Mujeres", "Hombres"),
                Edad != "TOTAL",
                CCAA.Prov == "Sevilla",
                Espanoles.Extranjeros == "Españoles") |>
  dplyr::rename(Poblacion = value) |>
  dplyr::select(Edad, Sexo, Poblacion)

DemBas_piramidePorc(datosPiramide,
                   Gtitulo = "Pirámide Población de Sevilla",
                   Gsubtitulo = "Año 2020 (españoles)",
                   GSegmentos = FALSE)
} # }
```
