# Descarga e importa archivos PC-Axis del INE (España)

Función convenience que combina la descarga de un archivo PC-Axis desde
el portal del INE con su importación directa a R. Utiliza la
codificación estándar del INE (CP437) sin necesidad de conversión.

## Usage

``` r
DemBas_import_px_INE(id, ficheropx = NULL)
```

## Arguments

- id:

  Cadena de texto con el identificador del archivo PC-Axis en el portal
  del INE.

- ficheropx:

  Cadena de texto opcional con el nombre del archivo donde guardar la
  descarga. Si es NULL, se crea un archivo temporal que se elimina al
  terminar la sesión. Por defecto NULL.

## Value

data.frame con los datos importados. Todas las columnas son de tipo
character para facilitar el procesamiento.

## See also

[`DemBas_url_px_INE`](https://DemographyBasic.github.io/reference/DemBas_url_px_INE.md)
para单独的 descarga,
[`DemBas_read_px`](https://DemographyBasic.github.io/reference/DemBas_read_px.md)
para importar archivos locales.

## Examples

``` r
if (FALSE) { # \dontrun{
# Importar usando archivo temporal
df2855 <- DemBas_import_px_INE("2855")
head(df2855)

# Importar guardando el archivo para reuse
df2855 <- DemBas_import_px_INE("2855", "2855_ine.px")
} # }
```
