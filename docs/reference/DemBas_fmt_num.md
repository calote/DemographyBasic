# Formatea números para presentación visual

Convierte un número a formato de texto con separadores de miles y
decimales personalizables. Utiliza la función `formatC` de R para
formateo consistente entre plataformas.

## Usage

``` r
DemBas_fmt_num(x, ndigitos = 5, quitacerosderecha = TRUE)
```

## Arguments

- x:

  Vector o valor numérico a formatear. Puede ser un solo número o un
  vector de números.

- ndigitos:

  Valor entero con el número máximo de dígitos significativos o
  decimales a mostrar. Por defecto 5.

- quitacerosderecha:

  Valor lógico. Si TRUE, elimina los ceros finales innecesarios en la
  parte decimal (ej. "123.50" se muestra como "123.5"). Si FALSE,
  mantiene todos los decimales especificados. Por defecto TRUE.

## Value

Vector de tipo character con los números formateados. Los valores
mantienen el separador de miles (espacio) y el punto como separador
decimal.

## Examples

``` r
DemBas_fmt_num(5654.0832)
#> [1] "5 654.0832"
DemBas_fmt_num(5654)
#> [1] "5 654"
DemBas_fmt_num(565400982.3000, 2, TRUE)
#> [1] "565 400 982.3"
DemBas_fmt_num(565400982.3000, 2, FALSE)
#> [1] "565 400 982.30"
DemBas_fmt_num(565400982.23713830023)
#> [1] "565 400 982.23714"
DemBas_fmt_num(565400982.23713830023, 2)
#> [1] "565 400 982.24"
```
