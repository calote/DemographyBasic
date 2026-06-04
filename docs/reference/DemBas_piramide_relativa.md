# Pirámide de población con valores relativos

Genera una pirámide de población mostrando densidades relativas
(porcentaje de población por unidad de edad). Los hombres se representan
a la izquierda del eje X y las mujeres a la derecha.

## Usage

``` r
DemBas_piramide_relativa(
  datos,
  titulo = "Pirámide de Población",
  unidad_densidad = "años",
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

- unidad_densidad:

  Unidad para el cálculo de densidad. Puede ser un número (ej. `5` para
  densidad por quinquenio) o un texto descriptivo (ej. `"quinquenio"`).
  Por defecto `"años"` (densidad anual).

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

  Texto personalizado para el eje X. Si es `NULL` (default), se genera
  automáticamente como `"Densidad Relativa (%/<unidad>)"`.

- etiqueta_y:

  Texto personalizado para el eje Y. Si es `NULL` (default), se usa
  `"Grupo de Edad"`.

## Value

Lista con dos elementos:

- grafico:

  Objeto `ggplot` con la pirámide de población.

- datos:

  Data frame procesado usado para construir el gráfico, con columnas
  `edad_grupo`, `sexo`, `poblacion`, `amplitud`, `porcentaje` y
  `densidad_ajustada`.

## Examples

``` r
if (FALSE) { # \dontrun{
datos <- data.frame(
  edad_grupo = c("65-69", "70-74", "75-84", "85-99"),
  amplitud   = c(5, 5, 10, 15),
  hombre     = c(520000, 350000, 280000, 120000),
  mujer      = c(560000, 385000, 340000, 180000)
)
r <- DemBas_piramide_relativa(datos, "Ejemplo", unidad_densidad = 5)
r$grafico
} # }
```
