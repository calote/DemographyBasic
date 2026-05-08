

library(tibble)
library(kableExtra)

### UTILIDADES


# Formatear números bonitos -----------------------------------------------

#' Formatea números para presentación visual
#'
#' Convierte un número a formato de texto con separadores de miles
#' y decimales personalizables. Utiliza la función \code{formatC} de R
#' para formateo consistente entre plataformas.
#'
#' @param x Vector o valor numérico a formatear. Puede ser un solo
#'   número o un vector de números.
#' @param ndigitos Valor entero con el número máximo de dígitos
#'   significativos o decimales a mostrar. Por defecto 5.
#' @param quitacerosderecha Valor lógico. Si TRUE, elimina los ceros
#'   finales innecesarios en la parte decimal (ej. "123.50" se muestra
#'   como "123.5"). Si FALSE, mantiene todos los decimales especificados.
#'   Por defecto TRUE.
#'
#' @return Vector de tipo character con los números formateados.
#'   Los valores mantienen el separador de miles (espacio) y el punto
#'   como separador decimal.
#'
#' @examples
#' DemBas_fmt_num(5654.0832)
#' DemBas_fmt_num(5654)
#' DemBas_fmt_num(565400982.3000, 2, TRUE)
#' DemBas_fmt_num(565400982.3000, 2, FALSE)
#' DemBas_fmt_num(565400982.23713830023)
#' DemBas_fmt_num(565400982.23713830023, 2)
#'
#' @export
DemBas_fmt_num <- function(x, ndigitos = 5, quitacerosderecha = TRUE) {
  # Sí redondea
  #prettyNum(x,big.mark=" ",decimal.mark = ".")
  formatC(x,format="f",big.mark = " ",drop0trailing=quitacerosderecha,digits = ndigitos,zero.print = T)
}

# sprintf(20.1, fmt='%.2f')
# sprintf('%.2f', 20.1)
#
# sprintf("%.0f%%", 66.666)
# sprintf("$%.2f", 99.999)



#' Redondea un vector numérico con corrección de error de punto flotante
#'
#' Redondea los valores de un vector numérico al número especificado de
#' decimales. Añade una pequeña corrección para evitar problemas de
#' representación de punto flotante que pueden causar redondeos
#' incorrectos (ej. 2.5 redondeado a 2 en lugar de 3).
#'
#' El ajuste \code{sign(x) * 1e-10} compensa el error de representación
#' en números de punto flotante que puede hacer que valores como
#' 2.9999999 no se redondeen correctamente.
#'
#' @param x Vector numérico con los valores a redondear.
#' @param digitos Valor entero con el número de decimales a mantener.
#'   Si es 0, redondea al entero más cercano. Por defecto 0.
#'
#' @return Vector numérico con los valores redondeados.
#'
#' @examples
#' v1 <- DemBas_redondear(rnorm(20), 2)
#' v1
#'
#' @export
DemBas_redondear <- function(x, digitos = 0) {
  round(x + sign(x) * 1e-10, digitos)
}



#' Redondea las columnas numéricas de un data.frame
#'
#' Aplica redondeo a todas las columnas de tipo numérico en un
#' data.frame, manteniendo las columnas no numéricas sin cambios.
#' Utiliza la misma corrección de punto flotante que
#' \code{\link{DemBas_redondear}}.
#'
#' @param df data.frame con columnas numéricas y no numéricas.
#' @param digitos Valor entero con el número de decimales a mantener
#'   en las columnas numéricas. Por defecto 0.
#'
#' @return data.frame con las columnas numéricas redondeadas y las
#'   columnas no numéricas sin cambios.
#'
#' @examples
#' df <- data.frame(nombres = sample(letters, 20),
#'                  x = rnorm(20),
#'                  y = rnorm(20))
#' df2 <- DemBas_redondear_df(df, 2)
#' df2
#'
#' @export
DemBas_redondear_df <- function(df, digitos = 0) {
  df2 = lapply(df, function(x) {
    if (is.numeric(x)) {
      round(x + sign(x) * 1e-10, digitos)
    } else {
      x
    }
  })
  return(as.data.frame(df2))
}




# Estaba en fichero: funciones_tablas.R  -------

library(knitr,warn.conflicts = F)
library(kableExtra)

#' Presenta un data.frame como tabla formateada (LaTeX o HTML)
#'
#' Genera una tabla formateada para presentación en documentos LaTeX
#' o HTML usando los paquetes knitr y kableExtra. La tabla se puede
#' diseñar en formato apaisado (landscape) y se divide automáticamente
#' en varias páginas si es necesario.
#'
#' @param datos1 data.frame o tibble con los datos a presentar.
#' @param scaption Cadena de texto opcional con el título (caption)
#'   de la tabla. Si es NULL, no se añade título. Por defecto NULL.
#' @param apaisadalatex Valor lógico. Si TRUE, la tabla se muestra
#'   en formato apaisado (horizontal) usando landscape. Por defecto FALSE.
#' @param variaspaginas Valor lógico. Si TRUE, la tabla se divide en
#'   múltiples páginas si es necesario (usando longtable). Por defecto TRUE.
#' @param fuentesize Valor numérico opcional con el tamaño de fuente
#'   para la tabla (ej. 8, 10, 12). Si es NULL, se usa el tamaño por
#'   defecto. Por defecto NULL.
#' @param CompletaAncho Valor lógico. Si TRUE, la tabla ocupa el ancho
#'   completo de la página. Por defecto FALSE.
#'
#' @return Objeto kableExtra de tipo R markdown/chunk que puede ser
#'   visualizado en documentos R Markdown, LaTeX o HTML según el formato
#'   de salida del documento.
#'
#' @examples
#' \dontrun{
#' # Ejemplo básico
#' DemBas_presentadf(mtcars)
#'
#' # Con título y formato apaisado
#' DemBas_presentadf(mtcars,
#'                  scaption = "Datos del dataset mtcars",
#'                  apaisadalatex = TRUE)
#' }
#'
#' @export
DemBas_presentadf <- function(datos1, scaption = NULL,
                               apaisadalatex = FALSE,
                               variaspaginas = TRUE,
                               fuentesize = NULL,
                               CompletaAncho = FALSE) {
  if (apaisadalatex) {
    if (variaspaginas) {
      if (!is.null(fuentesize)) {
        kable(datos1,longtable=TRUE,booktabs=TRUE, caption = scaption) |>
          kable_styling(
            bootstrap_options = c("striped", "hover",
                                  "condensed","responsive"),
            latex_options = c("striped","repeat_header","HOLD_position"),
            font_size = fuentesize,
            repeat_header_text="(continúa)") |>
          landscape()

      } else {
        kable(datos1,longtable=TRUE,booktabs=TRUE, caption = scaption) |>
          kable_styling(
            bootstrap_options = c("striped", "hover",
                                  "condensed","responsive"),
            latex_options = c("striped","repeat_header","HOLD_position"),
            repeat_header_text="(continúa)",
            full_width = CompletaAncho) |>
          landscape()
      }
    } else {
      kable(datos1,booktabs=TRUE, caption = scaption) |>
        kable_styling(
          bootstrap_options = c("striped", "hover",
                                "condensed","responsive"),
          latex_options = c("striped", "scale_down","HOLD_position"),
          full_width = CompletaAncho) |>
        landscape()
    }
  } else {
    if (!is.null(fuentesize)) {
      kable(datos1,longtable=TRUE,booktabs=TRUE, caption = scaption) |>
        kable_styling(
          bootstrap_options = c("striped", "hover",
                                "condensed","responsive"),
          latex_options = c("striped","repeat_header","HOLD_position"),
          font_size = fuentesize,
          repeat_header_text="(continúa)")
    } else {

      kable(datos1,longtable=TRUE,booktabs=TRUE, caption = scaption)  |>
        kable_styling(
          bootstrap_options = c("striped", "hover",
                                "condensed","responsive"),
          latex_options = c("striped","repeat_header","HOLD_position"),
          repeat_header_text="(continúa)",
          full_width = CompletaAncho)
    }
  }


}




# Tabla: kableExtra 1 -------------------------------------------------------------------------


#' Presenta un data.frame como tabla formateada (modelo 01)
#'
#' Versión alternativa de \code{\link{DemBas_presentadf}} con opciones
#' de personalización adicionales para el formato de la tabla. Incluye
#' soporte para centrado de cabeceras, alineación personalizada de
#' columnas y leyenda de tabla.
#'
#' @param df_tabla data.frame o tibble con los datos a presentar.
#' @param tablavariaspaginas Valor lógico. Si TRUE, la tabla se divide
#'   en múltiples páginas si es necesario (usando longtable). Por defecto TRUE.
#' @param tfuente Valor numérico con el tamaño de fuente para la tabla.
#'   Valores típicos son 8, 10 o 12. Por defecto 8.
#' @param leyendatabla Cadena de texto opcional con el título (caption)
#'   de la tabla. Si es NULL, no se añade leyenda. Por defecto NULL.
#' @param vCabecera Vector de caracteres opcional con los nombres de
#'   columnas a usar en lugar de los nombres originales del data.frame.
#'   Debe tener la misma longitud que el número de columnas. Por defecto NULL.
#' @param valigncol Cadena de texto opcional que especifica la alineación
#'   de cada columna (ej. "clclc" para izquierda, centro, izquierda, centro,
#'   centro). Si es NULL, se usa la alineación por defecto. Por defecto NULL.
#'
#' @return Objeto kableExtra de tipo R markdown/chunk con la tabla
#'   formateada lista para incluir en documentos LaTeX o HTML.
#'
#' @examples
#' \dontrun{
#' # Ejemplo básico
#' DemBas_presentadf_modelo01(mtcars)
#'
#' # Con leyenda y nombres de columnas personalizados
#' DemBas_presentadf_modelo01(mtcars,
#'                            tablavariaspaginas = TRUE,
#'                            tfuente = 10,
#'                            leyendatabla = "Vehículos del dataset mtcars",
#'                            vCabecera = c("Marca", "Millas/gal", "Cilindros",
#'                                          "Disp.", "Eje", "Peso"))
#' }
#'
#' @export
DemBas_presentadf_modelo01 <- function(df_tabla,
                                        tablavariaspaginas = TRUE,
                                        tfuente = 8,
                                        leyendatabla = NULL,
                                        vCabecera = NULL,
                                        valigncol = NULL) {

  tb01 = df_tabla
  options(knitr.kable.NA = '--',scipen=10)
  # colnames(tb01) = c("Edades","$P_x^{Esp}$","$D_x^{Esp}$","$m_x^{Esp}$",
  #                    "$P_x^{Can}$","$D_x^{Can}$","$m_x^{Can}$",
  #                    "$P_x^{Gal}$","$D_x^{Gal}$","$m_x^{Gal}$")
  if (!is.null(vCabecera)) {
    colnames(tb01) = vCabecera
  }

  # cols = c(2:length(tb01))
  # #df[,cols] = apply(df[,cols], 2, function(x) as.numeric(as.character(x)));
  # tb01[,cols] = apply(tb01[,cols], 2, function(x) as.character(x));
  if (!is.null(valigncol)) {
    if (!is.null(leyendatabla)) {
      ktb01 = tb01 %>%
        kable(escape=F,align=valigncol,
              booktabs=T,
              longtable = tablavariaspaginas,
              caption=leyendatabla)

    } else {
      ktb01 = tb01 %>%
        kable(escape=F,align=valigncol,
              booktabs=T,
              longtable = tablavariaspaginas)
    }
  } else {
    if (!is.null(leyendatabla)) {
      ktb01 = tb01 %>%
        kable(escape=F,
              booktabs=T,
              longtable = tablavariaspaginas,
              caption=leyendatabla)
    } else {
      ktb01 = tb01 %>%
        kable(escape=F,
              booktabs=T,
              longtable = tablavariaspaginas)

    }
  }
  ktb01 %>%
    kable_styling(latex_options = c("striped","HOLD_position","repeat_header"),
                  repeat_header_text = "\\textit{(continuación)}",
                  bootstrap_options = c("striped", "hover","condensed","responsive"),
                  position = "center",full_width = F,
                  font_size = tfuente) %>%
    row_spec(0, align = "c")
  # %>%  row_spec(nrow(tb01),bold=T)



}

