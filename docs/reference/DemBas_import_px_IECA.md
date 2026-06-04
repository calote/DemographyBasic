# Descarga e importa archivos PC-Axis del IECA (Andalucía)

Función convenience que combina la descarga de un archivo PC-Axis desde
el portal del IECA con su importación directa a R. Maneja
automáticamente la conversión de codificación y la creación de archivos
temporales si no se especifica un destino.

## Usage

``` r
DemBas_import_px_IECA(id, ficheropx = NULL)
```

## Arguments

- id:

  Cadena de texto con el identificador del archivo PC-Axis en el portal
  del IECA.

- ficheropx:

  Cadena de texto opcional con el nombre del archivo donde guardar la
  descarga. Si es NULL, se crea un archivo temporal que se elimina al
  terminar la sesión. Por defecto NULL.

## Value

data.frame con los datos importados. Todas las columnas son de tipo
character para facilitar el procesamiento.

## See also

[`DemBas_url_px_IECA`](https://DemographyBasic.github.io/reference/DemBas_url_px_IECA.md)
para单独的 descarga,
[`DemBas_read_px_encodingIECA`](https://DemographyBasic.github.io/reference/DemBas_read_px_encodingIECA.md)
para importar archivos locales.

## Examples

``` r
if (FALSE) { # \dontrun{
# Importar usando archivo temporal
df103965 <- DemBas_import_px_IECA("103965")
head(df103965)

# Importar guardando el archivo para reuse
df103965 <- DemBas_import_px_IECA("103965", "ieca_export_103965.px")
} # }
```
