# Pirámide superpuesta con perfiles de línea

Genera una pirámide de población superpuesta utilizando líneas en lugar
de barras, mostrando la evolución o comparación de dos o más períodos o
categorías. Los hombres se representan a la izquierda (valores
negativos) y las mujeres a la derecha (valores positivos), usando
facetas separadas y coordenadas volteadas.

## Usage

``` r
DemBas_piramide_superpuesta_lineas(
  datosPiramide,
  titulo = "Piramide superpuesta",
  subtitulo = NULL,
  colores = NULL,
  etiq.hombre = "Hombres",
  etiq.mujer = "Mujeres",
  tamano.linea = 0.8
)
```

## Arguments

- datosPiramide:

  data.frame o tibble con columnas:

  Edad

  :   Variable numérica con las edades simples

  Sexo

  :   Variable categórica con valores `"Hombres"` y `"Mujeres"`

  Poblacion

  :   Variable numérica con datos de población absolutos

  Caso

  :   Variable categórica que identifica cada período/categoría (ej. año
      de referencia)

- titulo:

  Cadena de texto con el título del gráfico. Por defecto
  `"Pirámide superpuesta"`.

- subtitulo:

  Cadena de texto con el subtítulo del gráfico. Por defecto `NULL`.

- colores:

  Vector nombrado con colores para cada valor de `Caso`. Por defecto
  `NULL` (usa la paleta por defecto de ggplot2).

- etiq.hombre:

  Cadena de texto con la etiqueta para hombres. Por defecto `"Hombres"`.

- etiq.mujer:

  Cadena de texto con la etiqueta para mujeres. Por defecto `"Mujeres"`.

- tamano.linea:

  Valor numérico para el grosor de las líneas. Por defecto `0.8`.

## Value

Objeto ggplot2 con la pirámide superpuesta de perfiles.

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(42)
edades <- 0:85
n <- length(edades)

base_2010 <- dnorm(edades, mean = 40, sd = 22) * 1000
base_2019 <- dnorm(edades, mean = 45, sd = 23) * 1100

datos <- rbind(
  data.frame(Edad = edades, Sexo = "Hombres", Caso = 2010,
             Poblacion = base_2010 * 0.48 * (1 + rnorm(n, 0, 0.02))),
  data.frame(Edad = edades, Sexo = "Mujeres", Caso = 2010,
             Poblacion = base_2010 * 0.52 * (1 + rnorm(n, 0, 0.02))),
  data.frame(Edad = edades, Sexo = "Hombres", Caso = 2019,
             Poblacion = base_2019 * 0.47 * (1 + rnorm(n, 0, 0.02))),
  data.frame(Edad = edades, Sexo = "Mujeres", Caso = 2019,
             Poblacion = base_2019 * 0.53 * (1 + rnorm(n, 0, 0.02)))
)

DemBas_piramide_superpuesta_lineas(
  datos,
  titulo = "Pirámide superpuesta",
  subtitulo = "Datos sintéticos 2010 y 2019",
  colores = c("2010" = "#4CA4DE", "2019" = "#D342CE")
)
} # }
```
