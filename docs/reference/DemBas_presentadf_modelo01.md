# Presenta un data.frame como tabla formateada (modelo 01)

Versión alternativa de
[`DemBas_presentadf`](https://DemographyBasic.github.io/reference/DemBas_presentadf.md)
con opciones de personalización adicionales para el formato de la tabla.
Incluye soporte para centrado de cabeceras, alineación personalizada de
columnas y leyenda de tabla.

## Usage

``` r
DemBas_presentadf_modelo01(
  df_tabla,
  tablavariaspaginas = TRUE,
  tfuente = 8,
  leyendatabla = NULL,
  vCabecera = NULL,
  valigncol = NULL
)
```

## Arguments

- df_tabla:

  data.frame o tibble con los datos a presentar.

- tablavariaspaginas:

  Valor lógico. Si TRUE, la tabla se divide en múltiples páginas si es
  necesario (usando longtable). Por defecto TRUE.

- tfuente:

  Valor numérico con el tamaño de fuente para la tabla. Valores típicos
  son 8, 10 o 12. Por defecto 8.

- leyendatabla:

  Cadena de texto opcional con el título (caption) de la tabla. Si es
  NULL, no se añade leyenda. Por defecto NULL.

- vCabecera:

  Vector de caracteres opcional con los nombres de columnas a usar en
  lugar de los nombres originales del data.frame. Debe tener la misma
  longitud que el número de columnas. Por defecto NULL.

- valigncol:

  Cadena de texto opcional que especifica la alineación de cada columna
  (ej. "clclc" para izquierda, centro, izquierda, centro, centro). Si es
  NULL, se usa la alineación por defecto. Por defecto NULL.

## Value

Objeto kableExtra de tipo R markdown/chunk con la tabla formateada lista
para incluir en documentos LaTeX o HTML.

## Examples

``` r
if (FALSE) { # \dontrun{
# Ejemplo básico
DemBas_presentadf_modelo01(mtcars)

# Con leyenda y nombres de columnas personalizados
DemBas_presentadf_modelo01(mtcars,
                           tablavariaspaginas = TRUE,
                           tfuente = 10,
                           leyendatabla = "Vehículos del dataset mtcars",
                           vCabecera = c("Marca", "Millas/gal", "Cilindros",
                                         "Disp.", "Eje", "Peso"))
} # }
```
