# Construye la URL para descargar archivos PC-Axis del IECA

Genera la URL necesaria para descargar un archivo PC-Axis desde el
portal de datos del Instituto de Estadística y Cartografía de Andalucía
(IECA) a partir de su identificador.

## Usage

``` r
DemBas_url_px_IECA(id, ficheropx)
```

## Arguments

- id:

  Cadena de texto con el identificador del archivo PC-Axis en el portal
  del IECA.

- ficheropx:

  Cadena de texto con el nombre del archivo donde se guardará la
  descarga. Si el archivo ya existe, será sobrescrito.

## Value

Cadena de texto con la URL del archivo PC-Axis.

## See also

[`DemBas_import_px_IECA`](https://DemographyBasic.github.io/reference/DemBas_import_px_IECA.md)
para descargar e importar en un solo paso.

## Examples

``` r
if (FALSE) { # \dontrun{
url <- DemBas_url_px_IECA("103965", "ieca_export_103965.px")
datos <- DemBas_read_px_encodingIECA("ieca_export_103965.px")
} # }
```
