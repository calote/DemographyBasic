# Lee archivos en formato PC-Axis (.px)

Importa datos desde archivos en formato PC-Axis utilizados por
instituciones estadísticas como el INE (España), Eurostat, y otras. Esta
función corrige un problema de memoria en sistemas Mac que afectaba a la
función original de pxR.

## Usage

``` r
DemBas_read_px(
  filename,
  encoding = NULL,
  na.strings = c("\".\"", "\"..\"", "\"...\"", "\"....\"", "\":\"")
)
```

## Arguments

- filename:

  Cadena de texto con la ruta al archivo .px a leer. Puede ser una ruta
  local o una URL.

- encoding:

  Cadena de texto opcional con la codificación del archivo. Si es NULL,
  se detecta automáticamente (CP437 o latin1). Por defecto NULL.

- na.strings:

  Vector de cadenas de texto que representan valores missing en el
  archivo. Por defecto c('"."', '".."', '"..."', '"...."', '":"').

## Value

data.frame con los datos importados. Las columnas содержат valores de
tipo character en lugar de factors para mayor facilidad de uso.

## References

PC-Axis es un formato de intercambio de datos estadísticos desarrollado
por Statistics Sweden y utilizado por oficinas de estadísticas
nacionales.

## Examples

``` r
if (FALSE) { # \dontrun{
# Importar un archivo .px del sistema de ejemplos del paquete
datos <- DemBas_read_px(system.file("examples/56940.px",
                                   package = "DemographyBasic"))
head(datos)

# Importar desde URL
# datos_url <- DemBas_read_px("https://www.ine.es/jaxiT3/files/t/es/px/2855.px")
} # }
```
