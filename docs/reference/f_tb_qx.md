# Funciones auxiliares para cálculo de tablas de vida

Estas funciones calculan diferentes columnas de una tabla de vida a
partir de las tasas de mortalidad centralizadas.

## Usage

``` r
f_tb_qx(Mx)

f_tb_px(qx)

f_tb_lx(px, l0 = 1e+05)

f_tb_dx(lx, qx)

f_tb_Lx(lx, dx, Mx)

f_tb_Tx(Lx)

f_tb_ex(Tx, lx)

f_tb_Sx(Lx, lx)
```

## Arguments

- Mx:

  Vector de tasas de mortalidad (para f_tb_Lx).

- qx:

  Vector de probabilidades de muerte (para f_tb_dx).

- px:

  Vector de probabilidades de supervivencia.

- l0:

  Población inicial o radix (para f_tb_lx). Por defecto 100000.

- lx:

  Vector de supervivientes (para f_tb_ex, f_tb_Sx).

- dx:

  Vector de defunciones en el intervalo.

- Lx:

  Vector de personas-año vividas (para f_tb_Tx, f_tb_ex, f_tb_Sx).
