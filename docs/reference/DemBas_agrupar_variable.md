# Agrupa una variable continua en grupos de edad quinquenales

Agrupa valores numéricos de edad en grupos quinquenales (0-4, 5-9,
10-14, ...) utilizando diferentes métodos de agrupación. Los grupos
resultantes son factores que pueden usarse directamente en tablas y
gráficos de pirámides poblacionales.

## Usage

``` r
DemBas_agrupar_variable(
  variable,
  metodo = 1,
  final = 85,
  vbreaks = NULL,
  vlabels = NULL,
  labelfinal = NULL
)
```

## Arguments

- variable:

  Vector numérico con los valores de edad a agrupar. Debe ser de tipo
  numeric o integer.

- metodo:

  Valor entero que indica el método de agrupación:

  1

  :   Grupos: 0, 1-4, 5-9, 10-14, ... (edad 0 separada)

  2

  :   Grupos: 0-4, 5-9, 10-14, 15-19, ... (todos quinquenales)

  Por defecto 1.

- final:

  Valor numérico con la edad final para el último grupo antes del grupo
  abierto (ej. 85 significa que el último grupo cerrado es 85-89 si
  metodo=1, o 80-84 si metodo=2). Por defecto 85.

- vbreaks:

  Vector numérico opcional con los puntos de corte personalizados para
  los grupos. Si se especifica, se ignoran los parámetros metodo y
  final. Por defecto NULL.

- vlabels:

  Vector de caracteres opcional con las etiquetas personalizadas para
  cada grupo definido por vbreaks. Por defecto NULL.

- labelfinal:

  Cadena de texto opcional con la etiqueta para el grupo final abierto
  (ej. "85+", "100+"). Si es NULL, se usa paste0(final, "+"). Por
  defecto NULL.

## Value

Factor con los grupos de edad resultantes de la agrupación. El nivel
final incluye todas las edades mayores o iguales al límite especificado.

## Examples

``` r
# Agrupar edades con método 1 (0 separado)
edades <- c(0, 2, 5, 23, 45, 67, 88, 95, 105)
grupos1 <- DemBas_agrupar_variable(edades, metodo = 1, final = 85)
grupos1
#> [1] 0     1-4   5-9   20-24 45-49 65-69 85+   85+   85+  
#> 19 Levels: 0 1-4 5-9 10-14 15-19 20-24 25-29 30-34 35-39 40-44 45-49 ... 85+

# Agrupar edades con método 2 (todos quinquenales)
grupos2 <- DemBas_agrupar_variable(edades, metodo = 2, final = 85)
grupos2
#> [1] 0-4   0-4   5-9   20-24 45-49 65-69 85+   85+   85+  
#> 18 Levels: 0-4 5-9 10-14 15-19 20-24 25-29 30-34 35-39 40-44 45-49 ... 85+

# Usar puntos de corte personalizados
grupos_custom <- DemBas_agrupar_variable(edades,
                                         vbreaks = c(0, 18, 35, 65, 150),
                                         vlabels = c("Niños", "Jóvenes",
                                                     "Adultos", "Mayores"))
grupos_custom
#> [1] Niños   Niños   Niños   Jóvenes Adultos Mayores Mayores Mayores Mayores
#> Levels: Niños Jóvenes Adultos Mayores
```
