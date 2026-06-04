# Lee los metadatos de un archivo PC-Axis del INE

Extrae la información de metadatos (cabeceras, variables, valores) de un
archivo PC-Axis publicado por el INE de España. Los metadatos incluyen
información sobre las variables, sus categorías y otros atributos del
conjunto de datos.

## Usage

``` r
DemBas_leer_metadatos_px_INE(ficheropx)
```

## Arguments

- ficheropx:

  Cadena de texto con la ruta al archivo .px del INE a procesar. El
  archivo debe estar codificado en Windows-1252.

## Value

data.frame con dos columnas:

- Claves:

  Nombre del atributo o variable (ej. "VALUES", "CODES", "TIME")

- Valores:

  Contenido del atributo (ej. los valores o categorías asociados)

## Examples

``` r
if (FALSE) { # \dontrun{
# Descargar e importar un archivo del INE
df2855 <- DemBas_import_px_INE("2855", "2855_ine.px")
head(df2855)

# Leer los metadatos del archivo
metadatos <- DemBas_leer_metadatos_px_INE("2855_ine.px")
head(metadatos)
} # }
```
