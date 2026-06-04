# Construye la URL para descargar archivos PC-Axis del INE (España)

Genera la URL necesaria para descargar un archivo PC-Axis desde el
portal del Instituto Nacional de Estadística (INE) de España a partir de
su identificador.

## Usage

``` r
DemBas_url_px_INE(id, ficheropx)
```

## Arguments

- id:

  Cadena de texto con el identificador del archivo PC-Axis en el portal
  del INE (por ejemplo, el código del juego de datos).

- ficheropx:

  Cadena de texto con el nombre del archivo donde se guardará la
  descarga. Si el archivo ya existe, será sobrescrito.

## Value

Cadena de texto con la URL del archivo PC-Axis.

## See also

[`DemBas_import_px_INE`](https://DemographyBasic.github.io/reference/DemBas_import_px_INE.md)
para descargar e importar en un solo paso.

## Examples

``` r
if (FALSE) { # \dontrun{
url <- DemBas_url_px_INE("2855", "2855_ine.px")
datos <- DemBas_read_px("2855_ine.px")
} # }
```
