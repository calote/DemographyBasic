# Corrige el número de decimales en datos importados del IECA

Los archivos PC-Axis del IECA codifican los valores numéricos con un
formato que requiere aplicar un factor de escala para obtener los
decimales correctos. Esta función implementa la corrección necesaria
basándose en el número de decimales especificado en los metadatos.

## Usage

``` r
DemBas_num_decimales_px_IECA(v1, num_decimales = 2)
```

## Arguments

- v1:

  Vector numérico con los valores a corregir. Estos valores provienen de
  la columna "value" de un data.frame importado con
  [`DemBas_import_px_IECA`](https://DemographyBasic.github.io/reference/DemBas_import_px_IECA.md).

- num_decimales:

  Valor entero con el número de decimales que deben tener los valores.
  Este valor se obtiene de los metadatos del archivo (campo "DECIMALS" o
  "SHOWDECIMALS"). Por defecto 2.

## Value

Vector numérico con los valores corregidos, expresados con el número de
decimales especificado.

## Details

El problema surge porque el IECA codifica los números de forma diferente
según su magnitud: valores grandes incluyen todos los decimales en la
parte entera, mientras que valores pequeños pueden tener formato mixto.

## Examples

``` r
if (FALSE) { # \dontrun{
# Importar datos del IECA
df19824 <- DemBas_import_px_IECA("19824", "ieca_19824.px")

# Obtener el número de decimales de los metadatos
metadatos <- DemBas_leer_metadatos_px_IECA("ieca_19824.px")
decimales_row <- metadatos[metadatos$Claves %in% c("DECIMALS", "SHOWDECIMALS"), ]
num_dec <- as.integer(decimales_row$Valores)

# Corregir los valores
df19824$value <- DemBas_num_decimales_px_IECA(df19824$value, num_dec)
head(df19824, 15)
} # }
```
