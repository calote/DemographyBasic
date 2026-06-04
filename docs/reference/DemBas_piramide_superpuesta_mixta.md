# Pirámide superpuesta mixta (barras + línea escalonada)

Genera una pirámide de población que combina barras para un período y
una línea escalonada para otro, permitiendo comparar la estructura por
edades entre dos momentos. Los hombres se representan a la izquierda
(valores negativos) y las mujeres a la derecha (positivos).

## Usage

``` r
DemBas_piramide_superpuesta_mixta(
  datosPiramide,
  Caso.referencia = NULL,
  titulo = "Piramide superpuesta mixta",
  subtitulo = NULL,
  colores.barra = c(Hombres = "#2c3e50", Mujeres = "#e74c3c"),
  colores.linea = c(Hombres = "black", Mujeres = "black"),
  alpha = 0.4,
  tamano.linea = 1,
  etiq.hombre = "Hombres",
  etiq.mujer = "Mujeres"
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

  :   Variable categórica que identifica cada período/categoría

- Caso.referencia:

  Valor de `Caso` que se representará como línea escalonada
  (referencia). Los demás valores se muestran como barras. Por defecto
  el primer nivel de `Caso`.

- titulo:

  Cadena de texto con el título del gráfico. Por defecto
  `"Pirámide superpuesta mixta"`.

- subtitulo:

  Cadena de texto con el subtítulo del gráfico. Por defecto `NULL`.

- colores.barra:

  Vector nombrado con colores de relleno para cada `Sexo` en las barras.
  Por defecto `c("Hombres" = "#2c3e50", "Mujeres" = "#e74c3c")`.

- colores.linea:

  Vector nombrado con colores de línea para cada `Sexo` en la línea
  escalonada. Por defecto `c("Hombres" = "black", "Mujeres" = "black")`.

- alpha:

  Valor numérico entre 0 y 1 para la transparencia de las barras. Por
  defecto `0.4`.

- tamano.linea:

  Valor numérico para el grosor de la línea escalonada. Por defecto `1`.

- etiq.hombre:

  Cadena de texto con la etiqueta para hombres. Por defecto `"Hombres"`.

- etiq.mujer:

  Cadena de texto con la etiqueta para mujeres. Por defecto `"Mujeres"`.

## Value

Objeto ggplot2 con la pirámide superpuesta mixta.

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(42)
edades <- 0:85
n <- length(edades)

base_2010 <- dnorm(edades, mean = 40, sd = 22) * 1000
base_2020 <- dnorm(edades, mean = 45, sd = 23) * 1100

datos <- rbind(
  data.frame(Edad = edades, Sexo = "Hombres", Caso = 2010,
             Poblacion = base_2010 * 0.48 * (1 + rnorm(n, 0, 0.02))),
  data.frame(Edad = edades, Sexo = "Mujeres", Caso = 2010,
             Poblacion = base_2010 * 0.52 * (1 + rnorm(n, 0, 0.02))),
  data.frame(Edad = edades, Sexo = "Hombres", Caso = 2020,
             Poblacion = base_2020 * 0.47 * (1 + rnorm(n, 0, 0.02))),
  data.frame(Edad = edades, Sexo = "Mujeres", Caso = 2020,
             Poblacion = base_2020 * 0.53 * (1 + rnorm(n, 0, 0.02)))
)

DemBas_piramide_superpuesta_mixta(
  datos,
  Caso.referencia = 2010,
  titulo = "Superposición mixta: 2010 (línea) vs 2020 (barras)"
)
} # }
```
