# Crea el perfil de la pirámide poblacional como líneas

Genera una visualización de la estructura demográfica usando líneas en
lugar de barras, mostrando el perfil de la pirámide poblacional. Útil
para comparar la forma de la distribución sin las barras sólidas.

## Usage

``` r
DemBas_piramide_ggplot2_linea(
  datosPiramide,
  porcentajes = TRUE,
  etiquetas = FALSE,
  etiquetas.size = 4,
  UsaCaso = FALSE,
  etiq.hombre = "Hombre",
  etiq.mujer = "Mujer",
  colorear = "Sexo",
  colores = NULL
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
  variable "Caso". Por defecto FALSE.

- etiq.hombre:

  Cadena de texto con la etiqueta para hombres. Por defecto "Hombre".

- etiq.mujer:

  Cadena de texto con la etiqueta para mujeres. Por defecto "Mujer".

- colorear:

  Cadena de texto que indica la variable para el color de las líneas:
  "Sexo", "Edad" o "Poblacion". Por defecto "Sexo".

- colores:

  Vector opcional de colores para las líneas.

## Value

Objeto ggplot2 con el perfil de la pirámide.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requiere datos de ejemplo del paquete
DemBas_piramide_ggplot2_linea(dfPir2017,
                              colorear = "Sexo",
                              etiq.hombre = "Hombres",
                              etiq.mujer = "Mujeres") +
  labs(title = "Perfil de la Pirámide de Población de España en 2017") +
  scale_x_discrete(breaks = seq(0, 105, by = 5),
                  labels = paste0(as.character(seq(0, 105, by = 5)), "")) +
  guides(colour = "none")
} # }
```
