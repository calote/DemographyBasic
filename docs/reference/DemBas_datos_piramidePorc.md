# Prepara datos de pirámide para visualización con porcentajes

Transforma los datos de población para que estén listos para graficar
una pirámide con porcentajes. Agrupa las edades en intervalos, calcula
porcentajes respecto al total y adiciona columnas derivadas.

## Usage

``` r
DemBas_datos_piramidePorc(
  datosPiramide,
  GEdad_final = 100,
  etiq.hombre = "Hombres"
)
```

## Arguments

- datosPiramide:

  data.frame o tibble con columnas:

  Edad

  :   Variable categórica o numérica (simples: 0, 1, 2,...)

  Sexo

  :   Variable categórica (factor)

  Poblacion

  :   Variable numérica (absolutos)

- GEdad_final:

  Valor numérico con la edad máxima final para el último grupo de edad.
  Por defecto 100.

- etiq.hombre:

  Cadena de texto con la etiqueta para hombres. Por defecto "Hombres".

## Value

Un tibble con columnas: Edad (grupo de edad, factor), Sexo (categoria de
sexo, factor), Poblacion (poblacion absoluta, numerico), Porcentajes
(porcentaje respecto al total, numerico), Pob (poblacion original antes
de aplicar signo). Los valores de poblacion para hombres tienen signo
negativo para su representacion en la piramide.
