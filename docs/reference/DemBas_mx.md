# Calcula las tasas centralizadas de mortalidad para edades simples

Calcula las tasas centralizadas de mortalidad (mx) para edades simples a
partir de defunciones y población media, incluyendo el tratamiento
especial para el grupo de menores de un año (TMI).

## Usage

``` r
DemBas_mx(Px, Dx, N0, D0)
```

## Arguments

- Px:

  Vector numérico con la población media por edad simple.

- Dx:

  Vector numérico con las defunciones por edad simple.

- N0:

  Número de nacimientos en el periodo (usados para calcular la TMI).

- D0:

  Número de defunciones de menores de un año en el periodo.

## Value

Vector numérico con las tasas centralizadas de mortalidad (mx) para cada
edad simple. El primer elemento corresponde a la tasa de mortalidad
infantil (D0/N0) y los restantes son Dx/Px. Los nombres del vector
indican la edad, con el último grupo etiquetado como "100+".

## Examples

``` r
# Datos ficticios
Px <- c(441881, rep(1627456, 100))
Dx <- c(1733, rep(1000, 100))
N0 <- 441881
D0 <- 1733
mx <- DemBas_mx(Px, Dx, N0, D0)
mx[1:5]
#>       0       1       2       3       4 
#> 0.00392 0.00061 0.00061 0.00061 0.00061 
```
