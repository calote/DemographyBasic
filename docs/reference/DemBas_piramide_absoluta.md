# Pirámide de población con valores absolutos

Genera una pirámide de población mostrando valores absolutos (densidad
de población por grupo etario). Los hombres se representan a la
izquierda del eje X y las mujeres a la derecha.

## Usage

``` r
DemBas_piramide_absoluta(
  datos,
  titulo = "Pirámide de Población",
  mostrar_limites = FALSE,
  colores = colores_defecto,
  intervalos = NULL,
  saltos_y = NULL,
  limite_x = NULL,
  etiqueta_x = NULL,
  etiqueta_y = NULL
)
```

## Arguments

- datos:

  Data frame con columnas obligatorias: `edad_grupo` (etiquetas de
  intervalo, ej. `"60-69"`), `amplitud` (años que abarca cada
  intervalo), `hombre` y `mujer` (población total de cada sexo en el
  intervalo).

- titulo:

  Título del gráfico. Por defecto `"Pirámide de Población"`.

- mostrar_limites:

  Lógico. Si `TRUE`, el eje Y muestra los límites de los intervalos (ej.
  `60, 70, 75, ...`) en vez de las etiquetas de rango (ej.
  `60-69, 70-74, ...`). Por defecto `FALSE`.

- colores:

  Vector nombrado con los colores para cada sexo:
  `c(hombre = "#color", mujer = "#color")`. Por defecto usa `#4CA4DE`
  para hombres y `#D342CE` para mujeres.

- intervalos:

  Vector de caracteres con intervalos destino para reagrupar los datos
  (ej. `"estandar1"` o un vector personalizado). Si es `NULL` (default),
  se usa la agrupación original de `datos`.

- saltos_y:

  Número entero que indica cada cuántos años mostrar un tick en el
  eje Y. Útil cuando los datos tienen granularidad anual para evitar
  etiquetas superpuestas. Por defecto `NULL` (desactivado).

- limite_x:

  Número que fija el valor máximo absoluto del eje X de forma simétrica
  (`c(-limite_x, limite_x)`). Permite comparar varias pirámides con la
  misma escala horizontal. Por defecto `NULL` (escala automática).

- etiqueta_x:

  Texto personalizado para el eje X. Si es `NULL` (default), se usa
  `"Población (densidad)"`.

- etiqueta_y:

  Texto personalizado para el eje Y. Si es `NULL` (default), se usa
  `"Grupo de Edad"`.

## Value

Lista con dos elementos:

- grafico:

  Objeto `ggplot` con la pirámide de población.

- datos:

  Data frame procesado usado para construir el gráfico, con columnas
  `edad_grupo`, `sexo`, `poblacion`, `amplitud` y `poblacion_ajustada`.

## Examples

``` r
if (FALSE) { # \dontrun{
datos <- data.frame(
  edad_grupo = c("60-69", "70-74", "75-84", "85-99"),
  amplitud   = c(10, 5, 10, 15),
  hombre     = c(450000, 280000, 320000, 150000),
  mujer      = c(480000, 310000, 380000, 200000)
)
r <- DemBas_piramide_absoluta(datos, "Ejemplo")
r$grafico
} # }
```
