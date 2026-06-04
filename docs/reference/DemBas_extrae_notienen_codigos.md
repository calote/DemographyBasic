# Identifica los valores que no tienen código asignado

Encuentra los elementos de una columna que no tienen un código
correspondiente en la columna de códigos (valores NA en la columna de
códigos).

## Usage

``` r
DemBas_extrae_notienen_codigos(columna, columnacodigos)
```

## Arguments

- columna:

  Vector de tipo character con los nombres originales.

- columnacodigos:

  Vector de tipo character o numeric con los códigos correspondientes.
  Los valores NA indican elementos sin código.

## Value

Vector de tipo character con los valores únicos de columna que no tienen
código asignado (es decir, cuyo código es NA).

## Examples

``` r
nombres <- c("Andorra", "España", "Francia", "Alemania")
codigos <- c(NA, "724", "250", NA)
sin_codigo <- DemBas_extrae_notienen_codigos(nombres, codigos)
sin_codigo
#> [1] "Andorra"  "Alemania"
```
