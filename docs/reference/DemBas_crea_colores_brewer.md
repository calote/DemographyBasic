# Genera una paleta de colores usando RColorBrewer

Crea un vector de colores para visualization de datos usando las paletas
disponibles en el paquete RColorBrewer. Permite seleccionar la paleta
por nombre o por número de índice.

## Usage

``` r
DemBas_crea_colores_brewer(cuantos, que_paletacolor = 3)
```

## Arguments

- cuantos:

  Valor entero con el número de colores a generar. Debe estar dentro del
  rango válido para la paleta seleccionada (generalmente entre 3 y 11).

- que_paletacolor:

  Puede ser:

  Valor entero

  :   Índice de la paleta (1 a 18) según la lista de paletas
      disponibles.

  Cadena de texto

  :   Nombre de una paleta válida de RColorBrewer (ej. "Blues",
      "YlOrRd", "PuBu").

  Por defecto 3 (paleta "BuPu").

## Value

Vector de caracteres con los códigos hexadecimales de los colores
generados.

## Examples

``` r
# Generar 5 colores de la paleta por defecto (BuPu)
colores1 <- DemBas_crea_colores_brewer(5)
colores1
#> [1] "#EDF8FB" "#B3CDE3" "#8C96C6" "#8856A7" "#810F7C"

# Generar 7 colores de la paleta "YlOrRd"
colores2 <- DemBas_crea_colores_brewer(7, que_paletacolor = "YlOrRd")
colores2
#> [1] "#FFFFB2" "#FED976" "#FEB24C" "#FD8D3C" "#FC4E2A" "#E31A1C" "#B10026"

# Usar índice de paleta (3 = BuPu)
colores3 <- DemBas_crea_colores_brewer(9, que_paletacolor = 3)
colores3
#> [1] "#F7FCFD" "#E0ECF4" "#BFD3E6" "#9EBCDA" "#8C96C6" "#8C6BB1" "#88419D"
#> [8] "#810F7C" "#4D004B"
```
