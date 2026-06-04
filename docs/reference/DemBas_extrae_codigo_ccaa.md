# Extrae el código numérico de comunidad autónoma del nombre

Obtiene los dígitos iniciales de código de comunidad autónoma de los
nombres que aparecen en los datos. Opcionalmente puede convertir los
códigos al formato SIANE (sumando 60 al código numérico).

## Usage

``` r
DemBas_extrae_codigo_ccaa(vCCAA, ConvierteCodSIANE = TRUE)
```

## Arguments

- vCCAA:

  Vector de tipo character con los nombres que incluyen el código de
  comunidad autónoma al inicio (ej. "01 Andalucía", "02 Aragón").

- ConvierteCodSIANE:

  Valor lógico. Si TRUE, convierte los códigos al formato SIANE sumando
  60 (ej. "01" -\> "61"). Si FALSE, devuelve los códigos originales. Por
  defecto TRUE.

## Value

Vector de caracteres o numéricos con los códigos de comunidad autónoma
extraídos y convertidos según el parámetro ConvierteCodSIANE.

## Examples

``` r
nombres_ccaa <- c("01 Andalucía", "02 Aragón", "03 Asturias")

# Formato original
codigos_orig <- DemBas_extrae_codigo_ccaa(nombres_ccaa,
                                          ConvierteCodSIANE = FALSE)
codigos_orig
#> 01 Andalucía    02 Aragón  03 Asturias 
#>         "01"         "02"         "03" 

# Formato SIANE
codigos_siane <- DemBas_extrae_codigo_ccaa(nombres_ccaa,
                                           ConvierteCodSIANE = TRUE)
codigos_siane
#> [1] 61 62 63
```
