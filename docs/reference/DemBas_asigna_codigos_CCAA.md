# Asigna un código personalizado a un valor específico de una columna

Reemplaza el código de un elemento específico en la columna de códigos
por un nuevo código proporcionado. Útil para estandarizar códigos o
corregir inconsistencias en los datos.

## Usage

``` r
DemBas_asigna_codigos_CCAA(columna, columna_codigos, etiqueta, nuevocodigo)
```

## Arguments

- columna:

  Vector de tipo character con los nombres originales.

- columna_codigos:

  Vector de tipo character o numeric con los códigos correspondientes.

- etiqueta:

  Valor de tipo character que identifica el elemento de columna cuyo
  código se desea modificar.

- nuevocodigo:

  Valor que se asignará como código al elemento identificado por
  etiqueta.

## Value

Vector con los códigos originales, donde el elemento correspondiente a
etiqueta ha sido reemplazado por nuevocodigo.

## Examples

``` r
nombres <- c("España", "Francia", "Alemania")
codigos <- c("724", "250", "276")
codigos_mod <- DemBas_asigna_codigos_CCAA(nombres, codigos, "España", "ES")
codigos_mod
#> [1] "ES"  "250" "276"
```
