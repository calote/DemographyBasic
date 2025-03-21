
# @title Calcula la tabla de vida para edades simples o completa
# @description Calcula la tabla de vida a partir de las tasas de mortalidad para edades simples: 0,1,2,...
# @param mx vector de tasas de mortalidad para edades simples: 0,1,2,...
#
# @returns devuelve un data.frame con la tabla de vida
# @examples
# Mx1000 = c(9.12160, 0.84807,0.49502,0.33352,0.27296,
# 0.23258,0.20229,0.19221,0.19225,0.18219,
# 0.18219,0.18223,0.19239,0.21268,0.25325,
# 0.31411,0.38518,0.44618,0.47682,0.48721,
# 0.48744,0.48768,0.48792,0.48816,0.48840,
# 0.48864,0.49907,0.49932,0.49957,0.51002,
# 0.52049,0.55140,0.58236,0.62360,0.67515,
# 0.72681,0.79907,0.89203,0.99549,1.09927,
# 1.22398,1.35944,1.50578,1.68380,1.87305,
# 2.07374,2.28609,2.52075,2.76762,3.04801,
# 3.34149,3.64844,3.99052,4.35799,4.76231,
# 5.19386,5.68611,6.20842,6.79514,7.42672,
# 8.12774,8.89053,9.74129,10.68500,11.73947,
# 12.91226,14.22468,15.71228,17.35403,19.16595,
# 21.20612,23.43628,25.96366,28.83038,32.10259,
# 35.83456,40.09691,44.96477,50.47392,56.71130,
# 63.73696,71.61161,80.38833,90.15169,100.87032,
# 112.56462,125.25733,138.92967,153.57492,169.22923,
# 185.87183,203.41806,222.05303,241.69867,262.24030,
# 283.83279,306.41026,329.80973,354.16667,379.65616,
# 406.15058,434.57189,462.12121,491.86992,832.50000)
# mx = Mx1000/1000
# tb01 = DemBas_tablavida(mx)
# tb01

#' @export
DemBas_tablavida2 = function(mx) {

  nn = length(mx)  # cuantos: 0,1,2, ..., 100+  -> nn=101
  ## Paso 1
  x = 0:(nn-1)

  ult = nn
  qx = rep(NA,ult)
  qx = (2*mx)/(2+mx)
  (qx[1] = mx[1]) # TMI = D^t_0/N^t
  #(qx[2:5] = (4*mx[2:5])/(1+4*(1-0.4)*mx[2:5]))
  (qx[ult] = 1)
  #  round(qx,5)

  ## Paso 2
  ##

  px = 1 - qx
  #  round(px,5)

  ## Paso 3
  ##

  dx = rep(NA,ult)
  lx = rep(NA,ult)
  lx[1] = 100000
  dx[1] = lx[1] * qx[1]

  for (i in 2:ult) {
    lx[i] = lx[i-1] - dx[i-1]
    dx[i] = lx[i] * qx[i]
  }
  #  round(dx,0)
  #  round(lx,0)


  ## Paso 4
  ##

  Lx = rep(NA,ult)
  Lx[6:(ult-1)] = ((lx[6:(ult-1)]+lx[7:ult]))/2
  Lx[1] = (lx[2] + 0.1*dx[1])
  Lx[2:5] = (lx[3:6] + 0.4*dx[2:5])
  Lx[ult] = lx[ult]/mx[ult]
  #  round(Lx,0)

  ##Paso 5
  ##'

  Tx = rep(NA,ult)
  Tx[ult] = Lx[ult]
  for (i in seq(ult-1,1,by=-1)) {
    Tx[i] = Tx[i+1] + Lx[i]
  }
  #  round(Tx,0)

  ##Paso 6
  ##'

  ex = Tx/lx
  #  round(ex,2)

  ##Paso 7
  ##'

  Sx = rep(NA,ult)
  #Sx[1] = (Lx[1] + Lx[2])/(5*lx[1])
  Sx[1] = (Lx[2])/(Lx[1])
  Sx[ult-1] = Lx[ult]/(Lx[ult-1]+Lx[ult])
  Sx[2:(ult-2)] = Lx[3:(ult-1)]/Lx[2:(ult-2)]
  #  round(Sx,5)

  ##Tabla de mortalidad
  ##'

  tablamortalidad = cbind(x,round(mx,5),round(qx,5),round(px,5),round(lx,0),
                          round(dx,0),round(Lx,0),
                          round(Tx,0),round(ex,2),round(Sx,5))
  colnames(tablamortalidad) = c("x","mx","qx","dx","lx","px","Lx","Tx","ex","Sx")
  df.tablamortalidad = as.data.frame(tablamortalidad)
  return(df.tablamortalidad)

}



#' @title Calcula las tasas especificas de mortalidad para edades simples
#' @description Calcula las tasas especificas de mortalidad para edades simplex a partir de las defunciones y población
#' @param Px vector de población media por edad
#' @param Dx vector de defunciones por edad
#' @param N0 nacimientos en el periodo
#' @param D0 defunciones en el periodo
#'
#' @returns devuelve un vector con las tasas especificas de mortalidad
#' @export
DemBas_mx = function(Px,Dx,N0,D0) {
  nn = length(Px)  # cuantos: 0,1,2, ..., 100+  -> nn=101
  x = c(0:(nn-1))
  mx0 = D0/N0
  mx = round(Dx/Px,5)
  mx[1] = round(mx0,5)
  names(mx) = x
  names(mx)[nn] = paste0(names(mx)[nn],"+")
  return(mx)
}







# @title Calcula la tabla de vida para edades agrupadas
# @description Calcula la tabla de vida abreviada a partir de las tasas de mortalidad para edades agrupadas: 0,1,2,...
# @param mx vector de tasas de mortalidad para edades simples: 0,1,2,...
#
# @returns devuelve un data.frame con la tabla de vida
# @export
# @examples
# (mx0 = 1733/441881) # TMI = D^t_0/N^t
# # Defunciones de menores de un año durante 2003: 1733
# # Nacimientos en España en 2003: 441881
# mx = c(mx0,0.00027, 0.00013, 0.00016, 0.00043, 0.00057, 0.00059, 0.00081,
#      0.00115, 0.00174, 0.00258, 0.00376, 0.00569, 0.00818, 0.01346,
#      0.02206, 0.03844, 0.06981, 0.12872, 0.21674, 0.31705, 0.48258)
# tv = DemBas_tablavida_abreviada2(mx)
DemBas_tablavida_abreviada2 = function(mx) {

  ## Paso 1
  x = c(0,1,seq(5,100,by=5))

  ult = length(mx)
  qx = rep(NA,ult)
  qx = (2*5*mx)/(2+5*mx)
  (qx[1] = mx[1]) # TMI = D^t_0/N^t
  (qx[2] = (4*mx[2])/(1+4*(1-0.4)*mx[2]))
  (qx[ult] = 1)
  #  round(qx,5)

  ## Paso 2
  ##

  dx = rep(NA,ult)
  lx = rep(NA,ult)
  lx[1] = 100000
  dx[1] = lx[1] * qx[1]

  for (i in 2:ult) {
    lx[i] = lx[i-1] - dx[i-1]
    dx[i] = lx[i] * qx[i]
  }
  #  round(dx,0)
  #  round(lx,0)

  ## Paso 3
  ##

  px = 1 - qx
  #  round(px,5)

  ## Paso 4
  ##

  Lx = rep(NA,ult)
  Lx[3:(ult-1)] = (5*(lx[3:(ult-1)]+lx[4:ult]))/2
  Lx[1] = 1 * (lx[2] + 0.1*dx[1])
  Lx[2] = 4 * (lx[3] + 0.4*dx[2])
  Lx[ult] = lx[ult]/mx[ult]
  #  round(Lx,0)

  ##Paso 5
  ##'

  Tx = rep(NA,ult)
  Tx[ult] = Lx[ult]
  for (i in seq(ult-1,1,by=-1)) {
    Tx[i] = Tx[i+1] + Lx[i]
  }
  #  round(Tx,0)

  ##Paso 6
  ##'

  ex = Tx/lx
  #  round(ex,2)

  ##Paso 7
  ##'

  Sx = rep(NA,ult)
  Sx[1] = (Lx[1] + Lx[2])/(5*lx[1])
  Sx[2] = (Lx[3]/(Lx[1]+Lx[2]))
  Sx[ult-1] = Lx[ult]/(Lx[ult-1]+Lx[ult])
  Sx[3:(ult-2)] = Lx[4:(ult-1)]/Lx[3:(ult-2)]
  #  round(Sx,5)

  ##Tabla de mortalidad
  ##'

  tablamortalidad = cbind(x,round(mx,5),round(qx,5),round(px,5),
                          round(lx,0),round(dx,0),round(Lx,0),
                          round(Tx,0),round(ex,2),round(Sx,5))
  colnames(tablamortalidad) = c("x","mx","qx","dx","lx","px","Lx","Tx","ex","Sx")
  df.tablamortalidad = as.data.frame(tablamortalidad)
  return(df.tablamortalidad)

}




#' @title Calcula las tasas especificas de mortalidad para edades agrupadas
#' @description Calcula las tasas especificas de mortalidad para edades agrupadas a partir de las defunciones y población
#' @param Px vector de población media por edad
#' @param Dx vector de defunciones por edad
#' @param N0 nacimientos en el periodo
#' @param D0 defunciones en el periodo
#'
#' @returns devuelve un vector con las tasas especificas de mortalidad
#' @export
#' @examples
#' x = c(0,1,seq(5,100,by=5))
#' Px = c(NA,1627456,1938350,2104636,2388049,3070467,3614444,3545550,
#'        3431304,3182840,2791972,2498361,2334676,1953022,1978465,
#'        1898370,1492487,974162,495260,203924,46078,5139)
#' Dx = c(NA,442,254,341,1032,1746,2136,2872,
#'        2933,5545,7193,9401,13294,15972,26636,
#'        41879,57377,68007,63007,44198,14609,2480)
#' N0 = 441881 # Nacimientos en España en 2003: 441881
#' D0 = 1733 # Defunciones de menores de un año durante 2003: 1733
#' mx = DemBas_mx_abreviada(Px,Dx,N0,D0)
#' mx
DemBas_mx_abreviada = function(Px,Dx,N0,D0) {

  x = c(0,1,seq(5,100,by=5))
  mx0 = D0/N0
  mx = round(Dx/Px,5)
  mx[1] = round(mx0,5)
  return(mx)
}

# tablamx = cbind(x,Px,Dx,mx)
# colnames(tablamx) = c("x","Px","Dx","mx")
# tablamx

# tabla de vida completa (Rowland) ------------------------------------------------------------

# Mx expresadas sin multiplicar por 1000 (valores < 1)
f_tb_qx = function(Mx) {
  qx = (2*Mx)/(2+Mx)
  qx[length(qx)] = 1
  qx
}

f_tb_px = function(qx) {
  px = 1-qx
  px
}

f_tb_lx = function(px,l0=100000) {
  lx = rep(NA,length(px))
  lx[1] = l0
  for (i in 2:length(px)) {
    lx[i] = lx[i-1] * px[i-1]
  }
  lx
}

f_tb_dx = function(lx,qx) {
  dx = lx * qx
  dx
}

f_tb_Lx = function(lx,dx,Mx) {
  Lx = lx - 0.5*dx
  Lx[1] = 0.3*lx[1] + 0.7*lx[2]
  Lx[2] = 0.4*lx[2] + 0.6*lx[3]
  Lx[length(Lx)] = lx[length(lx)]/Mx[length(Mx)]
  Lx
}

f_tb_Tx = function(Lx) {
  Tx = cumsum(rev(Lx))
  Tx = rev(Tx)
  Tx
}

f_tb_ex = function(Tx,lx) {
  ex = Tx/lx
  ex
}

f_tb_Sx = function(Lx,lx) {
  ult = length(Lx)
  Sx = rep(NA,ult)
  Sx[1] = (Lx[2])/(Lx[1])
  Sx[ult-1] = Lx[ult]/(Lx[ult-1]+Lx[ult])
  Sx[2:(ult-2)] = Lx[3:(ult-1)]/Lx[2:(ult-2)]
  Sx

}

#' @title Calcula la tabla de vida para edades simples o completa
#' @description Calcula la tabla de vida a partir de las tasas de mortalidad para edades simples: 0,1,2,...
#' @param mx vector de tasas de mortalidad para edades simples: 0,1,2,...
#' @param l0 población inicial o radix de la tabla
#' @param redondeo si TRUE redondea los valores
#' @param muestraSx si TRUE muestra la columna Sx (no incluye SN)
#'
#' @returns devuelve un data.frame con la tabla de vida
#' @Import tibble tibble
#' @examples
#' Mx1000 = c(9.12160, 0.84807,0.49502,0.33352,0.27296,
#' 0.23258,0.20229,0.19221,0.19225,0.18219,
#' 0.18219,0.18223,0.19239,0.21268,0.25325,
#' 0.31411,0.38518,0.44618,0.47682,0.48721,
#' 0.48744,0.48768,0.48792,0.48816,0.48840,
#' 0.48864,0.49907,0.49932,0.49957,0.51002,
#' 0.52049,0.55140,0.58236,0.62360,0.67515,
#' 0.72681,0.79907,0.89203,0.99549,1.09927,
#' 1.22398,1.35944,1.50578,1.68380,1.87305,
#' 2.07374,2.28609,2.52075,2.76762,3.04801,
#' 3.34149,3.64844,3.99052,4.35799,4.76231,
#' 5.19386,5.68611,6.20842,6.79514,7.42672,
#' 8.12774,8.89053,9.74129,10.68500,11.73947,
#' 12.91226,14.22468,15.71228,17.35403,19.16595,
#' 21.20612,23.43628,25.96366,28.83038,32.10259,
#' 35.83456,40.09691,44.96477,50.47392,56.71130,
#' 63.73696,71.61161,80.38833,90.15169,100.87032,
#' 112.56462,125.25733,138.92967,153.57492,169.22923,
#' 185.87183,203.41806,222.05303,241.69867,262.24030,
#' 283.83279,306.41026,329.80973,354.16667,379.65616,
#' 406.15058,434.57189,462.12121,491.86992,832.50000)
#' mx = Mx1000/1000
#' tb01 = DemBas_tablavida_completa(mx)
#' tb01
#'
#' @export
DemBas_tablavida_completa = function(Mx,l0=100000,redondeo=TRUE,muestraSx = TRUE) {
  noredondeo=!redondeo
  edades = as.character((1:length(Mx))-1)
  edades[length(edades)] = paste0(edades[length(edades)],"+")
  qx = f_tb_qx(Mx)
  px = f_tb_px(qx)
  lx = f_tb_lx(px,l0)
  dx = f_tb_dx(lx,qx)
  Lx = f_tb_Lx(lx,dx,Mx)
  Tx = f_tb_Tx(Lx)
  ex = f_tb_ex(Tx,lx)
  Sx = f_tb_Sx(Lx,lx)
  if (!noredondeo) {
    tb = tibble(Edad = edades,
                Mx1000 = round(Mx*1000,5),
                qx = round(qx,5),
                px = round(px,5),
                lx = round(lx,0),
                dx = round(dx,0),
                Lx = round(Lx,0),
                Tx = round(Tx,0),
                ex = round(ex,2),
                Sx = round(Sx,5))
  } else {
    tb = tibble(Edad = edades,
                Mx1000 = Mx*1000,
                qx = qx,
                px = px,
                lx = lx,
                dx = dx,
                Lx = Lx,
                Tx = Tx,
                ex = ex,
                Sx = Sx)
  }
  if (!muestraSx) {
    tb = tb[,-10]
  }
  tb
}



#' @title Calcula la tabla de vida para edades simples o completa pero simulando cálculos con una calculadora
#' @description Calcula la tabla de vida a partir de las tasas de mortalidad para edades simples: 0,1,2,... y simula los cálculos con una calculadora
#' @param mx vector de tasas de mortalidad para edades simples: 0,1,2,...
#' @param l0 población inicial o radix de la tabla
#' @param muestraSx si TRUE muestra la columna Sx (no incluye SN)
#'
#' @returns devuelve un data.frame con la tabla de vida
#' @Import tibble tibble
#' @examples
#' Mx1000 = c(9.12160, 0.84807,0.49502,0.33352,0.27296,
#' 0.23258,0.20229,0.19221,0.19225,0.18219,
#' 0.18219,0.18223,0.19239,0.21268,0.25325,
#' 0.31411,0.38518,0.44618,0.47682,0.48721,
#' 0.48744,0.48768,0.48792,0.48816,0.48840,
#' 0.48864,0.49907,0.49932,0.49957,0.51002,
#' 0.52049,0.55140,0.58236,0.62360,0.67515,
#' 0.72681,0.79907,0.89203,0.99549,1.09927,
#' 1.22398,1.35944,1.50578,1.68380,1.87305,
#' 2.07374,2.28609,2.52075,2.76762,3.04801,
#' 3.34149,3.64844,3.99052,4.35799,4.76231,
#' 5.19386,5.68611,6.20842,6.79514,7.42672,
#' 8.12774,8.89053,9.74129,10.68500,11.73947,
#' 12.91226,14.22468,15.71228,17.35403,19.16595,
#' 21.20612,23.43628,25.96366,28.83038,32.10259,
#' 35.83456,40.09691,44.96477,50.47392,56.71130,
#' 63.73696,71.61161,80.38833,90.15169,100.87032,
#' 112.56462,125.25733,138.92967,153.57492,169.22923,
#' 185.87183,203.41806,222.05303,241.69867,262.24030,
#' 283.83279,306.41026,329.80973,354.16667,379.65616,
#' 406.15058,434.57189,462.12121,491.86992,832.50000)
#' mx = Mx1000/1000
#' tb01 = DemBas_tablavida_completa_calculadora(mx)
#' tb01
#'
#' @export
DemBas_tablavida_completa_calculadora = function(Mx,l0=100000,muestraSx = TRUE) {
  edades = as.character((1:length(Mx))-1)
  edades[length(edades)] = paste0(edades[length(edades)],"+")
  Mx = DemBas_redondear(Mx,5)
  qx = f_tb_qx(Mx)
  qx = DemBas_redondear(qx,5)
  px = f_tb_px(qx)
  px = DemBas_redondear(px,5)
  lx = f_tb_lx(px,l0)
  lx = DemBas_redondear(lx,0)
  dx = f_tb_dx(lx,qx)
  dx = DemBas_redondear(dx,0)
  Lx = f_tb_Lx(lx,dx,Mx)
  Lx = DemBas_redondear(Lx,0)
  Tx = f_tb_Tx(Lx)
  Tx = DemBas_redondear(Tx,0)
  ex = f_tb_ex(Tx,lx)
  ex = DemBas_redondear(ex,2)
  Sx = f_tb_Sx(Lx,lx)
  Sx = DemBas_redondear(Sx,5)
  tb = tibble(Edad = edades,
              Mx1000 = Mx*1000,
              qx = qx,
              px = px,
              lx = lx,
              dx = dx,
              Lx = Lx,
              Tx = Tx,
              ex = ex,
              Sx = Sx)
  if (!muestraSx) {
    tb = tb[,-10]
  }
  tb

}


# tabla de vida abreviada (Rowland) -----------------------------------------------------------

f_tba_nqx = function(nMx,vn) {
  nqx = (2*vn*nMx)/(2+vn*nMx)
  nqx[length(nqx)] = 1
  nqx
}

f_tba_npx = function(nqx) {
  npx = 1-nqx
  npx
}

f_tba_lx = function(npx,l0=100000) {
  lx = rep(NA,length(npx))
  lx[1] = l0
  for (i in 2:length(npx)) {
    lx[i] = lx[i-1] * npx[i-1]
  }
  lx
}

f_tba_ndx = function(lx,nqx) {
  ndx = lx * nqx
  ndx
}

f_tba_nLx = function(lx,nMx,vn) {
  nLx = rep(NA,length(lx))
  for (i in 3:(length(lx)-1)) {
    nLx[i] = (vn[i]/2)*(lx[i]+lx[i+1])
  }
  nLx[1] = 0.3*lx[1] + 0.7*lx[2]
  nLx[2] = (4/2)*(lx[2] + lx[3])
  nLx[length(nLx)] = lx[length(lx)]/nMx[length(nMx)]
  nLx
}

f_tba_Tx = function(nLx) {
  Tx = cumsum(rev(nLx))
  Tx = rev(Tx)
  Tx
}

f_tba_ex = function(Tx,lx) {
  ex = Tx/lx
  ex
}

f_tba_Sx = function(Lx,lx) {
  ult = length(Lx)
  Sx = rep(NA,ult)
  Sx[1] = (Lx[1] + Lx[2])/(5*lx[1])
  Sx[2] = (Lx[3]/(Lx[1]+Lx[2]))
  Sx[ult-1] = Lx[ult]/(Lx[ult-1]+Lx[ult])
  Sx[3:(ult-2)] = Lx[4:(ult-1)]/Lx[3:(ult-2)]
  Sx

}

#' @title Calcula la tabla de vida para edades agrupadas
#' @description Calcula la tabla de vida abreviada a partir de las tasas de mortalidad para edades agrupadas: 0,1,2,...
#' @param mx vector de tasas de mortalidad para edades simples: 0,1,2,...
#' @param l0 población inicial o radix de la tabla
#' @param redondeo si TRUE redondea los valores
#' @param muestraSx si TRUE muestra la columna Sx
#'
#' @returns devuelve un data.frame con la tabla de vida
#' @export
#' @examples
#' (mx0 = 1733/441881) # TMI = D^t_0/N^t
#' # Defunciones de menores de un año durante 2003: 1733
#' # Nacimientos en España en 2003: 441881
#' mx = c(mx0,0.00027, 0.00013, 0.00016, 0.00043, 0.00057, 0.00059, 0.00081,
#'      0.00115, 0.00174, 0.00258, 0.00376, 0.00569, 0.00818, 0.01346,
#'      0.02206, 0.03844, 0.06981, 0.12872, 0.21674, 0.31705, 0.48258)
#' tv = DemBas_tablavida_abreviada(mx)
DemBas_tablavida_abreviada = function(nMx,l0=100000,redondeo=TRUE,muestraSx = TRUE) {
  noredondeo=!redondeo
  vn = rep(5,length(nMx))
  vn[1] = 1
  vn[2] = 4
  edades = c(0,cumsum(vn)[1:(length(vn)-1)])
  vn[length(nMx)] = 1000  # no se usará
  edades = as.character(edades)
  edades[length(edades)] = paste0(edades[length(edades)],"+")
  nqx = f_tba_nqx(nMx,vn)
  npx = f_tba_npx(nqx)
  lx = f_tba_lx(npx,l0)
  ndx = f_tba_ndx(lx,nqx)
  nLx = f_tba_nLx(lx,nMx,vn)
  Tx = f_tba_Tx(nLx)
  ex = f_tba_ex(Tx,lx)
  Sx = f_tba_Sx(nLx,lx)
  if (!noredondeo) {
    tba = tibble(Edad = edades,
                 n = vn,
                 nMx1000 = round(nMx*1000,5),
                 nqx = round(nqx,5),
                 npx = round(npx,5),
                 lx = round(lx,0),
                 ndx = round(ndx,0),
                 nLx = round(nLx,0),
                 Tx = round(Tx,0),
                 ex = round(ex,2),
                 Sx = round(Sx,5))
  } else {
    tba = tibble(Edad = edades,
                 n = vn,
                 nMx1000 = nMx*1000,
                 nqx = nqx,
                 npx = npx,
                 lx = lx,
                 ndx = ndx,
                 nLx = nLx,
                 Tx = Tx,
                 ex = ex,
                 Sx = Sx)
  }
  if (!muestraSx) {
    tba = tba[,-11]
  }
  tba
}


#' @title Calcula la tabla de vida para edades agrupadas pero simulando cálculos con una calculadora
#' @description Calcula la tabla de vida abreviada a partir de las tasas de mortalidad para edades agrupadas: 0,1,2,... y simula los cálculos con una calculadora
#' @param mx vector de tasas de mortalidad para edades simples: 0,1,2,...
#' @param l0 población inicial o radix de la tabla
#' @param muestraSx si TRUE muestra la columna Sx
#'
#' @returns devuelve un data.frame con la tabla de vida
#' @export
#' @examples
#' (mx0 = 1733/441881) # TMI = D^t_0/N^t
#' # Defunciones de menores de un año durante 2003: 1733
#' # Nacimientos en España en 2003: 441881
#' mx = c(mx0,0.00027, 0.00013, 0.00016, 0.00043, 0.00057, 0.00059, 0.00081,
#'      0.00115, 0.00174, 0.00258, 0.00376, 0.00569, 0.00818, 0.01346,
#'      0.02206, 0.03844, 0.06981, 0.12872, 0.21674, 0.31705, 0.48258)
#' tv = DemBas_tablavida_abreviada_calculadora(mx)
DemBas_tablavida_abreviada_calculadora = function(nMx,l0=100000,muestraSx = TRUE) {
  vn = rep(5,length(nMx))
  vn[1] = 1
  vn[2] = 4
  edades = c(0,cumsum(vn)[1:(length(vn)-1)])
  vn[length(nMx)] = 1000  # no se usará
  edades = as.character(edades)
  edades[length(edades)] = paste0(edades[length(edades)],"+")
  nMx = DemBas_redondear(nMx,5)
  nqx = f_tba_nqx(nMx,vn)
  nqx = DemBas_redondear(nqx,5)
  npx = f_tba_npx(nqx)
  npx = DemBas_redondear(npx,5)
  lx = f_tba_lx(npx,l0)
  lx = DemBas_redondear(lx,0)
  ndx = f_tba_ndx(lx,nqx)
  ndx = DemBas_redondear(ndx,0)
  nLx = f_tba_nLx(lx,nMx,vn)
  nLx = DemBas_redondear(nLx,0)
  Tx = f_tba_Tx(nLx)
  Tx = DemBas_redondear(Tx,0)
  ex = f_tba_ex(Tx,lx)
  ex = DemBas_redondear(ex,2)
  Sx = f_tba_Sx(nLx,lx)
  Sx = DemBas_redondear(Sx,5)
  tba = tibble(Edad = edades,
               n = vn,
               nMx1000 = nMx*1000,
               nqx = nqx,
               npx = npx,
               lx = lx,
               ndx = ndx,
               nLx = nLx,
               Tx = Tx,
               ex = ex,
               Sx = Sx)
  if (!muestraSx) {
    tba = tba[,-11]
  }
  tba
}
