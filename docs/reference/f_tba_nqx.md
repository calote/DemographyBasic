# Funciones auxiliares para cálculo de tablas de vida abreviadas

Estas funciones calculan diferentes columnas de una tabla de vida
abreviada a partir de las tasas de mortalidad centralizadas para grupos
de edad.

## Usage

``` r
f_tba_nqx(nMx, vn)

f_tba_npx(nqx)

f_tba_lx(npx, l0 = 1e+05)

f_tba_ndx(lx, nqx)

f_tba_nLx(lx, nMx, vn)

f_tba_Tx(nLx)

f_tba_ex(Tx, lx)

f_tba_Sx(Lx, lx)
```

## Arguments

- nMx:

  Vector de tasas de mortalidad (para f_tba_nLx).

- vn:

  Vector con las longitudes de cada intervalo de edad.

- nqx:

  Vector de probabilidades de muerte (para f_tba_ndx).

- npx:

  Vector de probabilidades de supervivencia (para f_tba_lx).

- l0:

  Población inicial o radix (para f_tba_lx). Por defecto 100000.

- lx:

  Vector de supervivientes (para f_tba_ex, f_tba_Sx).

- nLx:

  Vector de personas-año vividas (para f_tba_Tx, f_tba_ex, f_tba_Sx).

- Lx:

  Vector de personas-año vividas (para f_tba_Sx).
