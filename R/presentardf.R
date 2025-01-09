

library(tibble)
library(kableExtra)

### UTILIDADES


# Formatear números bonitos -----------------------------------------------

#' @title DemBas_fmt_num
#' @description Formatea un número para presentación
#'
#' @param x 
#' @param ndigitos 
#' @param quitacerosderecha 
#'
#' @returns devuelve un character con el número formateado
#' @examples
#' DemBas_fmt_num(5654.0832)
#' DemBas_fmt_num(5654)
#' DemBas_fmt_num(565400982.3000,2,T)
#' DemBas_fmt_num(565400982.3000,2,F)
#' DemBas_fmt_num(565400982.23713830023)
#' DemBas_fmt_num(565400982.23713830023,2)
#' @export
DemBas_fmt_num = function(x,ndigitos=5,quitacerosderecha=TRUE) {
  # Sí redondea
  #prettyNum(x,big.mark=" ",decimal.mark = ".")
  formatC(x,format="f",big.mark = " ",drop0trailing=quitacerosderecha,digits = ndigitos,zero.print = T)
}

# sprintf(20.1, fmt='%.2f')
# sprintf('%.2f', 20.1)
# 
# sprintf("%.0f%%", 66.666)
# sprintf("$%.2f", 99.999)







# Estaba en fichero: funciones_tablas.R  -------

library(knitr,warn.conflicts = F)
library(kableExtra)

#' @title DemBas_presentadf
#' @description Presenta un data.frame en formato LaTeX o HTML con kableExtra
#'
#' @param datos1 
#' @param scaption 
#' @param apaisadalatex 
#' @param variaspaginas 
#' @param fuentesize 
#' @param CompletaAncho 
#'
#' @returns devuelve una tabla para ser presentada en formato LaTeX o HTML
#' @examples
#' DemBas_presentadf(mtcars)
#' @export
DemBas_presentadf = function(datos1,scaption = NULL,
                              apaisadalatex=FALSE,
                              variaspaginas=TRUE,
                              fuentesize=NULL, # 8, 2, 12, ...
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


#' @title DemBas_presentadf_modelo01
#' @description Presenta un data.frame en formato LaTeX o HTML con kableExtra
#'
#' @param df_tabla 
#' @param tablavariaspaginas 
#' @param tfuente 
#' @param leyendatabla 
#' @param vCabecera 
#' @param valigncol 
#'
#' @returns
#' @examples
#' DemBas_presentadf_modelo01(mtcars)
#' @export
DemBas_presentadf_modelo01 = function(df_tabla,
                                      tablavariaspaginas = T,
                                      tfuente = 8,
                                      leyendatabla = NULL,
                                      vCabecera=NULL,valigncol=NULL) {
  
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

