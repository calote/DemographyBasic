# Lee archivos PC-Axis del IECA (Andalucía) con codificación automática

Importa datos desde archivos en formato PC-Axis publicados por el
Instituto de Estadística y Cartografía de Andalucía (IECA). Maneja
automáticamente la conversión de codificación UTF-8 a Windows-1252, que
es la codificación típica de estos archivos.

## Usage

``` r
DemBas_read_px_encodingIECA(ficheropx)
```

## Arguments

- ficheropx:

  Cadena de texto con la ruta al archivo .px del IECA a leer. Debe estar
  en formato UTF-8.

## Value

data.frame con los datos importados, con todas las columnas convertidas
a tipo character.

## See also

[`DemBas_url_px_IECA`](https://DemographyBasic.github.io/reference/DemBas_url_px_IECA.md)
para descargar archivos desde la web del IECA.

## Examples

``` r
if (FALSE) { # \dontrun{
# Descargar primero el archivo desde la web del IECA
url <- DemBas_url_px_IECA("103965", "ieca_export_103965.px")
datos <- DemBas_read_px_encodingIECA("ieca_export_103965.px")
} # }
```
