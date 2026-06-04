# Crea pirámides poblacionales superpuestas

Genera una visualización donde múltiples pirámides poblacionales se
superponen en el mismo gráfico, permitiendo comparar diferentes períodos
o categorías simultáneamente. Útil para observar cambios temporales en
la estructura demográfica.

## Usage

``` r
DemBas_piramide_superpuestas_ggplot2(
  datosPiramide,
  porcentajes = TRUE,
  etiquetas = FALSE,
  etiquetas.size = 4,
  colores = NULL,
  transparente = FALSE,
  alfa = 0.1,
  bar.size = 1,
  etiq.hombre = "Hombre",
  etiq.mujer = "Mujer",
  porc.X.ini = 0,
  porc.X.by = 0.2
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

  :   Variable categórica que identifica cada pirámide (ej. período
      temporal)

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

  Vector opcional de colores para cada pirámide.

- transparente:

  Valor lógico. Si TRUE, las barras tienen transparencia (alpha). Por
  defecto FALSE.

- alfa:

  Valor numérico entre 0 y 1 para la transparencia de las barras cuando
  transparente=TRUE. Por defecto 0.1.

- bar.size:

  Valor numérico para el grosor del borde de las barras. Por defecto 1.

- etiq.hombre:

  Cadena de texto con la etiqueta para hombres. Por defecto "Hombre".

- etiq.mujer:

  Cadena de texto con la etiqueta para mujeres. Por defecto "Mujer".

- porc.X.ini:

  Valor numérico para el límite inferior del eje Y. Por defecto 0.

- porc.X.by:

  Valor numérico para el intervalo de los ticks del eje Y. Por defecto
  0.2.

## Value

Objeto ggplot2 con las pirámides superpuestas.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requiere datos de ejemplo del paquete
dfej02a <- DemBas_read_px(system.file("examples/9663.px",
                                      package = "DemographyBasic"))

dfPir2002 <- dfej02a |>
  dplyr::filter(Periodo == "1 de enero de  2002",
                !Sexo == "Ambos sexos",
                !Edad.simple %in% c("100 y más años", "Total")) |>
  dplyr::select(Edadchar = Edad.simple, Sexo, Poblacion = value)
dfPir2002$Edad <- DemBas_extrae_codigo_provincia(dfPir2002$Edadchar)
dfPir2002$Edad <- factor(dfPir2002$Edad, levels = unique(dfPir2002$Edad))
dfPir2002$Poblacion[is.na(dfPir2002$Poblacion)] <- 0

dfPir2002y2017 <- rbind(dfPir2002, dfPir2017)
dfPir2002y2017$Caso <- c(rep(2002, nrow(dfPir2002)),
                         rep(2017, nrow(dfPir2017)))

DemBas_piramide_superpuestas_ggplot2(dfPir2002y2017,
                                     etiq.hombre = "Hombres",
                                     etiq.mujer = "Mujeres",
                                     transparente = TRUE) +
  labs(title = "Pirámides de Población de España en 2002 y 2017") +
  scale_x_discrete(breaks = seq(0, 105, by = 5),
                  labels = paste0(as.character(seq(0, 105, by = 5)), ""))
} # }
```
