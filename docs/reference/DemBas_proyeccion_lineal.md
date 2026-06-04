# Proyecta población por edad y sexo (modelo lineal o exponencial)

## Usage

``` r
DemBas_proyeccion_lineal(
  datos,
  anos_proyeccion = 5,
  tasa_crecimiento = 0.01,
  metodo = "exponencial"
)
```

## Arguments

- datos:

  Data frame inicial con: edad, sexo, poblacion

- anos_proyeccion:

  Número de años a proyectar

- tasa_crecimiento:

  Tasa de crecimiento anual (defecto 0.01 = 1

  metodo"lineal" o "exponencial"

List con datos originales y proyecciones Proyecta población por edad y
sexo (modelo lineal o exponencial) Proyección simple: asume tasa
constante sin cambios en estructura etaria. Para proyecciones realistas,
usar métodos de componentes.
