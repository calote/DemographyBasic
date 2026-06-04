# Crea pirámide poblacional con porcentajes, etiquetas y generaciones

Genera una pirámide de población avanzada que incluye: porcentajes de
población por grupo de edad, etiquetas con valores numéricos, líneas
divisorias para separar jóvenes/adultos/mayores, y un eje secundario con
las generaciones (años de nacimiento) correspondientes. También puede
presentar un resumen estadístico con índices demográficos.

## Usage

``` r
DemBas_piramidePorc_Generaciones(
  pop3,
  Ano_ref = 2020,
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
  GpresentaResumen = TRUE,
  GSegmentos = TRUE,
  GHombresColor = "#5BB4E5",
  GMujeresColor = "#DE61D8",
  GSegmentosColor = "#00ff00",
  Gguardar = FALSE,
  Garchivo = "piramide.png",
  Gwidth = 12,
  Gheight = 10
)
```

## Arguments

- pop3:

  data.frame o tibble con columnas:

  Edad

  :   Variable categórica con grupos de edad (factor)

  Sexo

  :   Variable categórica ("Hombres" o "Mujeres")

  Porcentajes

  :   Variable numérica con los datos de población

  Esta función requiere que los datos ya estén procesados por
  [`DemBas_datos_piramidePorc`](https://DemographyBasic.github.io/reference/DemBas_datos_piramidePorc.md).

- Ano_ref:

  Valor numérico con el año de referencia para calcular las
  generaciones. Por defecto 2020.

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

  Valor numérico con el límite izquierdo de la escala del eje Y. Por
  defecto -4.5.

- Gext_der:

  Valor numérico con el límite derecho de la escala del eje Y. Por
  defecto 4.5.

- Glimite:

  Valor numérico con el umbral mínimo para mostrar etiquetas. Por
  defecto 0.5.

- Gsizeletra:

  Valor numérico para el tamaño de la letra de las etiquetas. Por
  defecto 2.5.

- GpresentaResumen:

  Valor lógico. Si TRUE, muestra un resumen estadístico en el pie del
  gráfico con información sobre porcentajes por sexo, grupos de edad e
  índice de envejecimiento. Por defecto TRUE.

- GSegmentos:

  Valor lógico. Si TRUE, añade segmentos verticales para separar grupos
  de edad. Por defecto TRUE.

- GHombresColor:

  Cadena de texto con el color para hombres. Por defecto "#5BB4E5"
  (azul).

- GMujeresColor:

  Cadena de texto con el color para mujeres. Por defecto "#DE61D8"
  (rosa).

- GSegmentosColor:

  Cadena de texto con el color de los segmentos. Por defecto "#00ff00"
  (verde).

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

Objeto ggplot2 con la pirámide poblacional avanzada.

## References

Para información sobre índices demográficos y estructura de la
población, consulte manuales de demografía técnica.

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

g_pir1 <- DemBas_piramidePorc_Generaciones(datosPiramide,
                                          Gtitulo = "Pirámide Población de Sevilla",
                                          Gsubtitulo = "Año 2020 (españoles)")
g_pir1

g_pir2 <- DemBas_piramidePorc_Generaciones(datosPiramide,
                                          Gtitulo = "Pirámide Población de Sevilla",
                                          Gsubtitulo = "Año 2020 (españoles)",
                                          GSegmentos = FALSE,
                                          GpresentaResumen = FALSE)
g_pir2
} # }
```
