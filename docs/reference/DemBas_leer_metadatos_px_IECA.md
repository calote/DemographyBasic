# Lee los metadatos de un archivo PC-Axis del IECA (Andalucía)

Extrae la información de metadatos (cabeceras, variables, valores) de un
archivo PC-Axis publicado por el Instituto de Estadística y Cartografía
de Andalucía (IECA). Similar a
[`DemBas_leer_metadatos_px_INE`](https://DemographyBasic.github.io/reference/DemBas_leer_metadatos_px_INE.md)
pero adaptada a la codificación UTF-8 de los archivos del IECA.

## Usage

``` r
DemBas_leer_metadatos_px_IECA(ficheropx)
```

## Arguments

- ficheropx:

  Cadena de texto con la ruta al archivo .px del IECA a procesar. El
  archivo debe estar codificado en UTF-8.

## Value

data.frame con dos columnas:

- Claves:

  Nombre del atributo o variable (ej. "VALUES", "CODES", "TIME")

- Valores:

  Contenido del atributo (ej. los valores o categorías asociados)

## Examples

``` r
if (FALSE) { # \dontrun{
# Descargar e importar un archivo del IECA
df103965 <- DemBas_import_px_IECA("103965", "ieca_export_103965.px")
head(df103965)

# Leer los metadatos del archivo
metadatos <- DemBas_leer_metadatos_px_IECA("ieca_export_103965.px")
head(metadatos)
} # }
```
