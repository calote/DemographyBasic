library(tidyverse)
library(ggthemes)
library(glue)

#' Estructura de datos esperada para funciones de pirámides
#'
#' El data.frame o tibble de entrada debe contener las siguientes columnas:
#' \describe{
#'   \item{Edad}{Variable categórica (character o factor) con las edades
#'     (simples o grupos de edad)}
#'   \item{Sexo}{Variable categórica (character o factor) con los valores
#'     "Hombre" y "Mujer" (o etiquetas equivalentes)}
#'   \item{Poblacion}{Variable numérica con los valores de población (absolutos)}
#'   \item{Caso}{(Opcional) Variable categórica para pirámides compuestas
#'     por categorías}
#' }

#' Crea pirámides poblacionales usando ggplot2
#'
#' Genera una pirámide de población con barras enfrentadas para hombres y mujeres.
#' Admite opciones de personalización para colores, etiquetas, porcentajes y
#' esquema de color por edad, sexo o población.
#'
#' @param datosPiramide data.frame o tibble con columnas:
#'   \describe{
#'     \item{Edad}{Variable categórica con edades (simples o grupos)}
#'     \item{Sexo}{Variable categórica ("Hombre" o "Mujer")}
#'     \item{Poblacion}{Variable numérica (absolutos)}
#'     \item{Caso}{(Opcional) Variable categórica para pirámides compuestas}
#'   }
#' @param porcentajes Valor lógico. Si TRUE, muestra la población en
#'   porcentajes respecto al total. Por defecto TRUE.
#' @param etiquetas Valor lógico. Si TRUE, muestra etiquetas con los
#'   valores en las barras. Por defecto FALSE.
#' @param etiquetas.size Valor numérico que controla el tamaño del texto
#'   de las etiquetas. Por defecto 4.
#' @param UsaCaso Valor lógico. Si TRUE, calcula porcentajes por cada
#'   categoría de la variable "Caso". Por defecto FALSE.
#' @param etiq.hombre Cadena de texto con la etiqueta para hombres.
#'   Por defecto "Hombre".
#' @param etiq.mujer Cadena de texto con la etiqueta para mujeres.
#'   Por defecto "Mujer".
#' @param colorear Cadena de texto que indica la variable para el color
#'   de las barras: "Sexo", "Edad" o "Poblacion". Por defecto "Sexo".
#' @param colores Vector opcional de colores para las barras.
#' @param porc.X.ini Valor numérico para el límite inferior inicial del
#'   eje Y cuando se usan porcentajes. Por defecto 0.
#' @param porc.X.by Valor numérico para el intervalo de los ticks del
#'   eje Y cuando se usan porcentajes. Por defecto 0.2.
#'
#' @return Objeto ggplot2 con la pirámide poblacional.
#'
#' @examples
#' \dontrun{
#' dfej02a <- DemBas_read_px(system.file("examples/9663.px",
#'                                       package = "DemographyBasic"))
#' head(dfej02a)
#' tp1 <- dfej02a |>
#'   dplyr::filter(Periodo == "1 de enero de  2017",
#'                 Edad.simple == "Total") |>
#'   dplyr::select("Sexo", "value")
#'
#' dfPir2017 <- dfej02a |>
#'   dplyr::filter(Periodo == "1 de enero de  2017",
#'                 !Sexo == "Ambos sexos",
#'                 !Edad.simple %in% c("100 y más años", "Total")) |>
#'   dplyr::select(Edadchar = Edad.simple, Sexo, Poblacion = value)
#' dfPir2017$Edad <- DemBas_extrae_codigo_provincia(dfPir2017$Edadchar)
#' dfPir2017$Edad <- factor(dfPir2017$Edad, levels = unique(dfPir2017$Edad))
#' dfPir2017$Poblacion[is.na(dfPir2017$Poblacion)] <- 0
#'
#' DemBas_piramide_ggplot2(dfPir2017,
#'                         etiq.hombre = "Hombres",
#'                         etiq.mujer = "Mujeres") +
#'   labs(title = "Pirámide de Población de España en 2017") +
#'   scale_x_discrete(breaks = seq(0, 105, by = 5),
#'                   labels = paste0(as.character(seq(0, 105, by = 5)), ""))
#' }
#'
#' @export
DemBas_piramide_ggplot2 <- function(datosPiramide,
                                     porcentajes = TRUE,
                                     etiquetas = FALSE, etiquetas.size = 4,
                                     UsaCaso = FALSE,
                                     etiq.hombre = "Hombre", etiq.mujer = "Mujer",
                                     colorear = "Sexo", colores = NULL,
                                     porc.X.ini = 0, porc.X.by = 0.2) {
  #colnames(datos)[2] = "Genero"
  #browser()
  df2 = datosPiramide
  df2$Poblacion[df2$Sexo==etiq.hombre] = - df2$Poblacion[df2$Sexo==etiq.hombre]
  Gcadporcen = ""
  if (porcentajes) {
    Gcadporcen = "%"

    if (UsaCaso) {  # UsaCaso=TRUE -> ("Caso" %in% colnames(df2) )
      df3 = df2 %>%
        dplyr::group_by(Caso) %>%
        dplyr::summarise(
          Subtotal = sum(abs(Poblacion))
        )
      fCaso = function(x) {
        df3$Subtotal[df3$Caso==x]
      }

      vfCaso = function(x) {
        sapply(x,fCaso)
      }

      df2 = df2 %>%
        dplyr::mutate(
          Total = vfCaso(Caso),
          Porcentajes = 100*round(Poblacion/Total,4)
        )
    } else {
      df2 = df2 %>%
        dplyr::mutate(
          Total = sum(abs(Poblacion)),
          Porcentajes = 100*round(Poblacion/Total,4)
        )
    }
    df2 = df2 %>%
      dplyr::rename(
        Pob = Poblacion
      )
    df2$Poblacion = df2$Porcentajes
  }

  if (colorear=="Edad") {
    pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion, fill = Edad)) +
      geom_bar(data = subset(df2, Sexo == etiq.mujer), stat = "identity") +
      geom_bar(data = subset(df2, Sexo == etiq.hombre), stat = "identity")
  } else if (colorear=="Poblacion") {
    pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion, fill = Poblacion)) +
      geom_bar(data = subset(df2, Sexo == etiq.mujer), stat = "identity") +
      geom_bar(data = subset(df2, Sexo == etiq.hombre), stat = "identity")
  } else {
    pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion, fill = Sexo)) +
      geom_bar(data = subset(df2, Sexo == etiq.mujer), stat = "identity") +
      geom_bar(data = subset(df2, Sexo == etiq.hombre), stat = "identity")
  }

  if (etiquetas) {
    pyramidGH <- pyramidGH +
      geom_text(data = subset(df2, Sexo == etiq.hombre),
                aes(y = Poblacion,label = paste0((-1)*Poblacion,Gcadporcen)),
                size = etiquetas.size, hjust = 1) +
      geom_text(data = subset(df2, Sexo == etiq.mujer),
                aes(y = Poblacion,label = paste0(Poblacion,Gcadporcen)),
                size = etiquetas.size, hjust = 0)
  }

  if (porcentajes) {
    Vmax = round(max(abs(df2$Poblacion)),0) + 1
    pyramidGH <- pyramidGH +
      scale_y_continuous(breaks = c(seq(-Vmax, 0, porc.X.by), seq(porc.X.ini, Vmax, porc.X.by)),
                         labels = paste0(as.character(c(seq(Vmax, 0, -porc.X.by),
                                                        seq(porc.X.ini, Vmax, porc.X.by))), "%"))

  } else {
    tmp1 = max(round(range(df2$Poblacion/1000000),0))
    if (tmp1>=2) {
      pyramidGH <- pyramidGH +
        scale_y_continuous(labels = paste0(as.character(c(seq(tmp1, 0, -1),
                                                          seq(1, tmp1, 1))), "m"))
    } else {
      rango = range(abs(df2$Poblacion))
      ampli = diff(rango)
      vbr = round(c(seq(-rango[2],0,length.out = 3),
                    seq(1,rango[2],length.out = 3)),0)
      vbr2 = round(c(seq(rango[2],0,length.out = 3),
                     seq(1,rango[2],length.out = 3)),0)
      pyramidGH <- pyramidGH +
        scale_y_continuous(
          breaks = vbr,
          labels = paste0(as.character(vbr2), ""))

    }
  }

  if (!is.null(colores)) {
    pyramidGH = pyramidGH +
      scale_fill_manual(values=colores)
  }


  if (porcentajes) {
    pyramidGH <- pyramidGH +
      labs(y = "Porcentajes Población",
           x = "Edades")

  } else {
    pyramidGH <- pyramidGH +
      labs(y = "Población",
           x = "Edades")
  }

  if (colorear!="Sexo") {
    pyramidGH <- pyramidGH +
      guides(fill=FALSE)
  }

  pyramidGH <- pyramidGH +
    coord_flip()

  pyramidGH +
    theme_light()


}


#' Crea múltiples pirámides poblacionales enfrentadas en una cuadrícula
#'
#' Genera varias pirámides poblacionales organizadas en una cuadrícula de
#' facetas, permitiendo comparar pirámides de diferentes categorías o
#' períodos. Utiliza internamente \code{\link{DemBas_piramide_ggplot2}}.
#'
#' @param datosPiramide data.frame o tibble con columnas:
#'   \describe{
#'     \item{Edad}{Variable categórica con edades}
#'     \item{Sexo}{Variable categórica ("Hombre" o "Mujer")}
#'     \item{Poblacion}{Variable numérica (absolutos)}
#'     \item{Caso}{Variable categórica que define los grupos a comparar}
#'   }
#' @param porcentajes Valor lógico. Si TRUE, muestra la población en
#'   porcentajes. Por defecto TRUE.
#' @param etiquetas Valor lógico. Si TRUE, muestra etiquetas con los valores.
#'   Por defecto FALSE.
#' @param etiquetas.size Valor numérico para el tamaño del texto de
#'   las etiquetas. Por defecto 4.
#' @param UsaCaso Valor lógico. Si TRUE, calcula porcentajes por cada
#'   categoría de la variable "Caso". Por defecto TRUE.
#' @param etiq.hombre Cadena de texto con la etiqueta para hombres.
#'   Por defecto "Hombre".
#' @param etiq.mujer Cadena de texto con la etiqueta para mujeres.
#'   Por defecto "Mujer".
#' @param colorear Cadena de texto que indica la variable para el color
#'   de las barras: "Sexo", "Edad" o "Poblacion". Por defecto "Sexo".
#' @param colores Vector opcional de colores para las barras.
#' @param nfilas Valor numérico opcional para el número de filas de la
#'   cuadrícula de facetas.
#' @param ncols Valor numérico opcional para el número de columnas de la
#'   cuadrícula de facetas.
#'
#' @return Objeto ggplot2 con las pirámides enfrentadas en facetas.
#'
#' @seealso \code{\link{DemBas_piramide_ggplot2}} para la función base.
#'
#' @export
DemBas_piramides_enfrentadas_ggplot2 <- function(datosPiramide,
                                                porcentajes = TRUE,
                                                etiquetas = FALSE, etiquetas.size = 4,
                                                UsaCaso = TRUE,
                                                etiq.hombre = "Hombre", etiq.mujer = "Mujer",
                                                colorear = "Sexo", colores = NULL,
                                                nfilas = NULL, ncols = NULL) {
  #browser()
  p = DemBas_piramide_ggplot2(datosPiramide,porcentajes,
                            etiquetas,etiquetas.size,UsaCaso,
                            etiq.hombre,etiq.mujer,
                            colorear,colores)
  p = p +
    facet_wrap( ~Caso, nrow = nfilas,ncol = ncols)
  p

}




####
####
####


#' Crea pirámides poblacionales superpuestas
#'
#' Genera una visualización donde múltiples pirámides poblacionales se
#' superponen en el mismo gráfico, permitiendo comparar diferentes
#' períodos o categorías simultáneamente. Útil para observar cambios
#' temporales en la estructura demográfica.
#'
#' @param datosPiramide data.frame o tibble con columnas:
#'   \describe{
#'     \item{Edad}{Variable categórica con edades}
#'     \item{Sexo}{Variable categórica ("Hombre" o "Mujer")}
#'     \item{Poblacion}{Variable numérica (absolutos)}
#'     \item{Caso}{Variable categórica que identifica cada pirámide
#'       (ej. período temporal)}
#'   }
#' @param porcentajes Valor lógico. Si TRUE, muestra la población en
#'   porcentajes. Por defecto TRUE.
#' @param etiquetas Valor lógico. Si TRUE, muestra etiquetas con los
#'   valores. Por defecto FALSE.
#' @param etiquetas.size Valor numérico para el tamaño del texto de
#'   las etiquetas. Por defecto 4.
#' @param colores Vector opcional de colores para cada pirámide.
#' @param transparente Valor lógico. Si TRUE, las barras tienen
#'   transparencia (alpha). Por defecto FALSE.
#' @param alfa Valor numérico entre 0 y 1 para la transparencia de
#'   las barras cuando transparente=TRUE. Por defecto 0.1.
#' @param bar.size Valor numérico para el grosor del borde de las
#'   barras. Por defecto 1.
#' @param etiq.hombre Cadena de texto con la etiqueta para hombres.
#'   Por defecto "Hombre".
#' @param etiq.mujer Cadena de texto con la etiqueta para mujeres.
#'   Por defecto "Mujer".
#' @param porc.X.ini Valor numérico para el límite inferior del eje Y.
#'   Por defecto 0.
#' @param porc.X.by Valor numérico para el intervalo de los ticks del
#'   eje Y. Por defecto 0.2.
#'
#' @return Objeto ggplot2 con las pirámides superpuestas.
#'
#' @examples
#' \dontrun{
#' # Requiere datos de ejemplo del paquete
#' dfej02a <- DemBas_read_px(system.file("examples/9663.px",
#'                                       package = "DemographyBasic"))
#'
#' dfPir2002 <- dfej02a |>
#'   dplyr::filter(Periodo == "1 de enero de  2002",
#'                 !Sexo == "Ambos sexos",
#'                 !Edad.simple %in% c("100 y más años", "Total")) |>
#'   dplyr::select(Edadchar = Edad.simple, Sexo, Poblacion = value)
#' dfPir2002$Edad <- DemBas_extrae_codigo_provincia(dfPir2002$Edadchar)
#' dfPir2002$Edad <- factor(dfPir2002$Edad, levels = unique(dfPir2002$Edad))
#' dfPir2002$Poblacion[is.na(dfPir2002$Poblacion)] <- 0
#'
#' dfPir2002y2017 <- rbind(dfPir2002, dfPir2017)
#' dfPir2002y2017$Caso <- c(rep(2002, nrow(dfPir2002)),
#'                          rep(2017, nrow(dfPir2017)))
#'
#' DemBas_piramide_superpuestas_ggplot2(dfPir2002y2017,
#'                                      etiq.hombre = "Hombres",
#'                                      etiq.mujer = "Mujeres",
#'                                      transparente = TRUE) +
#'   labs(title = "Pirámides de Población de España en 2002 y 2017") +
#'   scale_x_discrete(breaks = seq(0, 105, by = 5),
#'                   labels = paste0(as.character(seq(0, 105, by = 5)), ""))
#' }
#'
#' @export
DemBas_piramide_superpuestas_ggplot2 <- function(datosPiramide,
                                                  porcentajes = TRUE,
                                                  etiquetas = FALSE, etiquetas.size = 4,
                                                  colores = NULL,
                                                  transparente = FALSE,
                                                  alfa = 0.1, bar.size = 1,
                                                  etiq.hombre = "Hombre", etiq.mujer = "Mujer",
                                                  porc.X.ini = 0, porc.X.by = 0.2) {
  #colnames(datos)[2] = "Genero"
  #browser()
  UsaCaso=TRUE
  df2 = datosPiramide
  if (!is.factor(df2$Sexo)) {
    df2$Sexo = factor(df2$Sexo)
  }
  df2$Caso = factor(df2$Caso)
  df2$Poblacion[df2$Sexo==etiq.hombre] = - df2$Poblacion[df2$Sexo==etiq.hombre]
  Gcadporcen = ""
  if (porcentajes) {
    Gcadporcen = "%"

    if (UsaCaso) {  # UsaCaso=TRUE -> ("Caso" %in% colnames(df2) )
      df3 = df2 %>%
        dplyr::group_by(Caso) %>%
        dplyr::summarise(
          Subtotal = sum(abs(Poblacion))
        )
      fCaso = function(x) {
        df3$Subtotal[df3$Caso==x]
      }

      vfCaso = function(x) {
        sapply(x,fCaso)
      }

      df2 = df2 %>%
        mutate(
          Total = vfCaso(Caso),
          Porcentajes = 100*round(Poblacion/Total,4)
        )
    } else {
      df2 = df2 %>%
        mutate(
          Total = sum(abs(Poblacion)),
          Porcentajes = 100*round(Poblacion/Total,4)
        )
    }
    df2 = df2 %>%
      dplyr::rename(
        Pob = Poblacion
      )
    df2$Poblacion = df2$Porcentajes
  }

  if (!transparente) {
    pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion,
                                 color = Caso,fill="transparent"))
  } else {
    pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion,
                                 color = Caso,fill=Caso))
  }

  UniCasos = unique(df2$Caso)
  for (i in 1:length(UniCasos)) {
    pyramidGH = pyramidGH +
      # geom_bar(data = subset(df2, Caso==UniCasos[i]),
      #        stat = "identity",fill="transparent")
      geom_bar(data = subset(df2, Caso==UniCasos[i]),
               stat = "identity",size=bar.size,alpha=alfa)


  }

  if (etiquetas) {
    pyramidGH <- pyramidGH +
      geom_text(data = subset(df2, Sexo == etiq.hombre),
                aes(y = Poblacion,label = paste0((-1)*Poblacion,Gcadporcen)),
                size = etiquetas.size, hjust = 1) +
      geom_text(data = subset(df2, Sexo == etiq.mujer),
                aes(y = Poblacion,label = paste0(Poblacion,Gcadporcen)),
                size = etiquetas.size, hjust = 0)
  }

  if (porcentajes) {
    Vmax = round(max(abs(df2$Poblacion)),0) + 1
    pyramidGH <- pyramidGH +
      scale_y_continuous(breaks = c(seq(-Vmax, 0, porc.X.by), seq(porc.X.ini, Vmax, porc.X.by)),
                         labels = paste0(as.character(c(seq(Vmax, 0, -porc.X.by),
                                                        seq(porc.X.ini, Vmax, porc.X.by))), "%"))

  } else {
    tmp1 = max(round(range(df2$Poblacion/1000000),0))
    if (tmp1>=2) {
      pyramidGH <- pyramidGH +
        scale_y_continuous(labels = paste0(as.character(c(seq(tmp1, 0, -1),
                                                          seq(1, tmp1, 1))), "m"))
    } else {
      rango = range(abs(df2$Poblacion))
      ampli = diff(rango)
      vbr = round(c(seq(-rango[2],0,length.out = 3),
                    seq(1,rango[2],length.out = 3)),0)
      vbr2 = round(c(seq(rango[2],0,length.out = 3),
                     seq(1,rango[2],length.out = 3)),0)
      pyramidGH <- pyramidGH +
        scale_y_continuous(
          breaks = vbr,
          labels = paste0(as.character(vbr2), ""))

    }
  }

  if (!is.null(colores)) {
    pyramidGH <- pyramidGH +
      scale_fill_manual(values=colores) +
      scale_color_manual(values=colores)
  }

  if (porcentajes) {
    pyramidGH <- pyramidGH +
      labs(y = "Porcentajes Población",
           x = "Edades")

  } else {
    pyramidGH <- pyramidGH +
      labs(y = "Población",
           x = "Edades")
  }

  pyramidGH <- pyramidGH +
    coord_flip()

  pyramidGH +
    theme_light()


}



####
####
####


#' Crea pirámides poblacionales compuestas por categorías
#'
#' Genera una pirámide de población donde cada barra está subdividida
#' en segmentos que representan diferentes categorías (ej. españoles/
#' extranjeros, nacionalidades). A diferencia de las pirámides
#' superpuestas, aquí los segmentos se apilan dentro de cada barra.
#'
#' @param datosPiramide data.frame o tibble con columnas:
#'   \describe{
#'     \item{Edad}{Variable categórica con edades}
#'     \item{Sexo}{Variable categórica ("Hombre" o "Mujer")}
#'     \item{Poblacion}{Variable numérica (absolutos)}
#'     \item{Caso}{Variable categórica que identifica cada segmento
#'       (ej. grupo de nacionalidad)}
#'   }
#' @param porcentajes Valor lógico. Si TRUE, muestra la población en
#'   porcentajes. Por defecto TRUE.
#' @param etiquetas Valor lógico. Si TRUE, muestra etiquetas con los
#'   valores. Por defecto FALSE.
#' @param etiquetas.size Valor numérico para el tamaño del texto de
#'   las etiquetas. Por defecto 4.
#' @param colores Vector opcional de colores para cada categoría.
#' @param ordeninverso Valor lógico. Si TRUE, invierte el orden de
#'   apilamiento de los segmentos. Por defecto FALSE.
#' @param alfa Valor numérico entre 0 y 1 para la transparencia de
#'   las barras. Por defecto 1 (sin transparencia).
#' @param bar.size Valor numérico para el grosor del borde de las
#'   barras. Por defecto 1.
#' @param etiq.hombre Cadena de texto con la etiqueta para hombres.
#'   Por defecto "Hombre".
#' @param etiq.mujer Cadena de texto con la etiqueta para mujeres.
#'   Por defecto "Mujer".
#'
#' @return Objeto ggplot2 con la pirámide compuesta.
#'
#' @export
DemBas_piramide_compuestasCateg_ggplot2 <- function(datosPiramide,
                                                     porcentajes = TRUE,
                                                     etiquetas = FALSE, etiquetas.size = 4,
                                                     colores = NULL, ordeninverso = FALSE,
                                                     alfa = 1, bar.size = 1,
                                                     etiq.hombre = "Hombre", etiq.mujer = "Mujer") {
  #colnames(datos)[2] = "Genero"
  #browser()
  UsaCaso=FALSE
  transparente=TRUE
  df2 = datosPiramide
  df2$Caso = factor(df2$Caso)
  df2$Poblacion[df2$Sexo==etiq.hombre] = - df2$Poblacion[df2$Sexo==etiq.hombre]
  Gcadporcen = ""
  if (porcentajes) {
    Gcadporcen = "%"

    df2 = df2 %>%
      mutate(
        Total = sum(abs(Poblacion)),
        Porcentajes = 100*round(Poblacion/Total,4)
      )

    df2 = df2 %>%
      dplyr::rename(
        Pob = Poblacion
      )
    df2$Poblacion = df2$Porcentajes
  }

  pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion,
                               fill=Caso))

  if (ordeninverso) {
    pyramidGH = pyramidGH +
      geom_bar(stat="identity",color="black",
               position = position_stack(reverse = TRUE),
               size=bar.size,alpha=alfa)
  } else {
    pyramidGH = pyramidGH +
      geom_bar(stat="identity",color="black",
               size=bar.size,alpha=alfa)
  }

  if (etiquetas) {
    pyramidGH <- pyramidGH +
      geom_text(data = subset(df2, Sexo == etiq.hombre),
                aes(y = Poblacion,label = paste0((-1)*Poblacion,Gcadporcen)),
                size = etiquetas.size, hjust = 1) +
      geom_text(data = subset(df2, Sexo == etiq.mujer),
                aes(y = Poblacion,label = paste0(Poblacion,Gcadporcen)),
                size = etiquetas.size, hjust = 0)
  }

  if (porcentajes) {
    Vmax = round(max(abs(df2$Poblacion)),0) + 1

    pyramidGH <- pyramidGH +
      scale_y_continuous(breaks = c(seq(-Vmax, 0, 1), seq(1, Vmax, 1)),
                         labels = paste0(as.character(c(seq(Vmax, 0, -1),
                                                        seq(1, Vmax, 1))), "%"))

  } else {
    tmp1 = max(round(range(df2$Poblacion/1000000),0))
    if (tmp1>=2) {
      pyramidGH <- pyramidGH +
        scale_y_continuous(labels = paste0(as.character(c(seq(tmp1, 0, -1),
                                                          seq(1, tmp1, 1))), "m"))
    } else {
      rango = range(abs(df2$Poblacion))
      ampli = diff(rango)
      vbr = round(c(seq(-rango[2],0,length.out = 3),
                    seq(1,rango[2],length.out = 3)),0)
      vbr2 = round(c(seq(rango[2],0,length.out = 3),
                     seq(1,rango[2],length.out = 3)),0)
      pyramidGH <- pyramidGH +
        scale_y_continuous(
          breaks = vbr,
          labels = paste0(as.character(vbr2), ""))

    }
  }

  if (!is.null(colores)) {
    pyramidGH <- pyramidGH +
      scale_fill_manual(values=colores)
  }

  if (porcentajes) {
    pyramidGH <- pyramidGH +
      labs(y = "Porcentajes Población",
           x = "Edades")

  } else {
    pyramidGH <- pyramidGH +
      labs(y = "Población",
           x = "Edades")
  }

  pyramidGH <- pyramidGH +
    coord_flip()

  pyramidGH +
    theme_light()


}


####
####
####



####
####
####


### Crear perfiles de pirámides

#datosPiramide = dfPir2017

#' Crea el perfil de la pirámide poblacional como líneas
#'
#' Genera una visualización de la estructura demográfica usando líneas
#' en lugar de barras, mostrando el perfil de la pirámide poblacional.
#' Útil para comparar la forma de la distribución sin las barras sólidas.
#'
#' @param datosPiramide data.frame o tibble con columnas:
#'   \describe{
#'     \item{Edad}{Variable categórica con edades}
#'     \item{Sexo}{Variable categórica ("Hombre" o "Mujer")}
#'     \item{Poblacion}{Variable numérica (absolutos)}
#'   }
#' @param porcentajes Valor lógico. Si TRUE, muestra la población en
#'   porcentajes. Por defecto TRUE.
#' @param etiquetas Valor lógico. Si TRUE, muestra etiquetas con los
#'   valores. Por defecto FALSE.
#' @param etiquetas.size Valor numérico para el tamaño del texto de
#'   las etiquetas. Por defecto 4.
#' @param UsaCaso Valor lógico. Si TRUE, calcula porcentajes por cada
#'   categoría de la variable "Caso". Por defecto FALSE.
#' @param etiq.hombre Cadena de texto con la etiqueta para hombres.
#'   Por defecto "Hombre".
#' @param etiq.mujer Cadena de texto con la etiqueta para mujeres.
#'   Por defecto "Mujer".
#' @param colorear Cadena de texto que indica la variable para el color
#'   de las líneas: "Sexo", "Edad" o "Poblacion". Por defecto "Sexo".
#' @param colores Vector opcional de colores para las líneas.
#'
#' @return Objeto ggplot2 con el perfil de la pirámide.
#'
#' @examples
#' \dontrun{
#' # Requiere datos de ejemplo del paquete
#' DemBas_piramide_ggplot2_linea(dfPir2017,
#'                               colorear = "Sexo",
#'                               etiq.hombre = "Hombres",
#'                               etiq.mujer = "Mujeres") +
#'   labs(title = "Perfil de la Pirámide de Población de España en 2017") +
#'   scale_x_discrete(breaks = seq(0, 105, by = 5),
#'                   labels = paste0(as.character(seq(0, 105, by = 5)), "")) +
#'   guides(colour = "none")
#' }
#'
#' @export
DemBas_piramide_ggplot2_linea <- function(datosPiramide,
                                           porcentajes = TRUE,
                                           etiquetas = FALSE, etiquetas.size = 4,
                                           UsaCaso = FALSE,
                                           etiq.hombre = "Hombre", etiq.mujer = "Mujer",
                                           colorear = "Sexo", colores = NULL) {
  #colnames(datos)[2] = "Genero"
  #browser()
  df2 = datosPiramide
  df2$Poblacion[df2$Sexo==etiq.hombre] = - df2$Poblacion[df2$Sexo==etiq.hombre]
  Gcadporcen = ""
  if (porcentajes) {
    Gcadporcen = "%"

    if (UsaCaso) {  # UsaCaso=TRUE -> ("Caso" %in% colnames(df2) )
      df3 = df2 %>%
        dplyr::group_by(Caso) %>%
        dplyr::summarise(
          Subtotal = sum(abs(Poblacion))
        )
      fCaso = function(x) {
        df3$Subtotal[df3$Caso==x]
      }

      vfCaso = function(x) {
        sapply(x,fCaso)
      }

      df2 = df2 %>%
        dplyr::mutate(
          Total = vfCaso(Caso),
          Porcentajes = 100*round(Poblacion/Total,4)
        )
    } else {
      df2 = df2 %>%
        dplyr::mutate(
          Total = sum(abs(Poblacion)),
          Porcentajes = 100*round(Poblacion/Total,4)
        )
    }
    df2 = df2 %>%
      dplyr::rename(
        Pob = Poblacion
      )
    df2$Poblacion = df2$Porcentajes
  }
  #browser()
  if (colorear=="Edad") {
    pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion, colour = Edad,group=Sexo))
    #pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion, fill = Edad,group=Sexo))
    #geom_bar(data = subset(df2, Sexo == etiq.mujer), stat = "identity") +
    #geom_bar(data = subset(df2, Sexo == etiq.hombre), stat = "identity")
  } else if (colorear=="Poblacion") {
    pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion, colour = Poblacion,group=Sexo))
    #pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion, fill = Poblacion,group=Sexo))
    #geom_bar(data = subset(df2, Sexo == etiq.mujer), stat = "identity") +
    #geom_bar(data = subset(df2, Sexo == etiq.hombre), stat = "identity")
  } else {
    pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion, colour = Sexo,group=Sexo))
    #pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion, fill = Sexo,group=Sexo))
    #geom_bar(data = subset(df2, Sexo == etiq.mujer), stat = "identity") +
    #geom_bar(data = subset(df2, Sexo == etiq.hombre), stat = "identity")
  }

  if (etiquetas) {
    pyramidGH <- pyramidGH +
      geom_text(data = subset(df2, Sexo == etiq.hombre),
                aes(y = Poblacion,label = paste0((-1)*Poblacion,Gcadporcen)),
                size = etiquetas.size, hjust = 1) +
      geom_text(data = subset(df2, Sexo == etiq.mujer),
                aes(y = Poblacion,label = paste0(Poblacion,Gcadporcen)),
                size = etiquetas.size, hjust = 0)
  }

  if (porcentajes) {
    Vmax = round(max(abs(df2$Poblacion)),0) + 1
    pyramidGH <- pyramidGH +
      scale_y_continuous(breaks = c(seq(-Vmax, 0, 1), seq(1, Vmax, 1)),
                         labels = paste0(as.character(c(seq(Vmax, 0, -1),
                                                        seq(1, Vmax, 1))), "%"))

  } else {
    tmp1 = max(round(range(df2$Poblacion/1000000),0))
    if (tmp1>=2) {
      pyramidGH <- pyramidGH +
        scale_y_continuous(labels = paste0(as.character(c(seq(tmp1, 0, -1),
                                                          seq(1, tmp1, 1))), "m"))
    } else {
      rango = range(abs(df2$Poblacion))
      ampli = diff(rango)
      vbr = round(c(seq(-rango[2],0,length.out = 3),
                    seq(1,rango[2],length.out = 3)),0)
      vbr2 = round(c(seq(rango[2],0,length.out = 3),
                     seq(1,rango[2],length.out = 3)),0)
      pyramidGH <- pyramidGH +
        scale_y_continuous(
          breaks = vbr,
          labels = paste0(as.character(vbr2), ""))

    }
  }

  if (!is.null(colores)) {
    pyramidGH = pyramidGH +
      scale_fill_manual(values=colores)
  }


  if (porcentajes) {
    pyramidGH <- pyramidGH +
      labs(y = "Porcentajes Población",
           x = "Edades")

  } else {
    pyramidGH <- pyramidGH +
      labs(y = "Población",
           x = "Edades")
  }

  if (colorear!="Sexo") {
    pyramidGH <- pyramidGH +
      guides(colour=FALSE)
    #guides(fill=FALSE)
  }

  if (etiquetas) {
    pyramidGH <- pyramidGH +
      coord_flip() +
      #geom_smooth(colour = "red", method = "glm", se = FALSE, show.legend = FALSE, size = 0.5) #+
      #geom_line(colour = "red") +
      geom_line() +
      geom_point()
    #scale_y_continuous(
    #           breaks = seq(-4, 4, by = 2),
    #           labels = c(rev(seq(0, 4, by = 2)), seq(2, 4, by = 2)))

  } else {
    pyramidGH <- pyramidGH +
      coord_flip() +
      #geom_smooth(colour = "red", method = "glm", se = FALSE, show.legend = FALSE, size = 0.5) #+
      #geom_line(colour = "red") +
      geom_line()
    #geom_point()

  }

  pyramidGH +
    theme_light()


}

#p = func_piramide_ggplot2_linea(datosPiramide2,porcentajes = TRUE,
#                                etiquetas = T,etiquetas.size = 3)
#p




# funciones pirámides con Porcentajes Nuevas ------

#' Convierte una variable factor o carácter a numérico
#'
#' Función auxiliar que convierte un vector de tipo factor o carácter
#' a formato numérico. Útil para preprocessar datos de edad en
#' pirámides poblacionales.
#'
#' @param x Vector de entrada (factor o carácter).
#'
#' @return Vector numérico con los valores convertidos.
#'
#' @noRd
func_factor_to_numeric <- function(x) {
  return(as.numeric(as.character(x)))
}

# datosPiramide
# columnas: Edad (fac,num, simples: 0,1,2,...), Sexo (factor), Poblacion (num)

#' Prepara datos de pirámide para visualización con porcentajes
#'
#' Transforma los datos de población para que estén listos para graficar
#' una pirámide con porcentajes. Agrupa las edades en intervalos,
#' calcula porcentajes respecto al total y adiciona columnas derivadas.
#'
#' @param datosPiramide data.frame o tibble con columnas:
#'   \describe{
#'     \item{Edad}{Variable categórica o numérica (simples: 0, 1, 2,...)}
#'     \item{Sexo}{Variable categórica (factor)}
#'     \item{Poblacion}{Variable numérica (absolutos)}
#'   }
#' @param GEdad_final Valor numérico con la edad máxima final para el
#'   último grupo de edad. Por defecto 100.
#' @param etiq.hombre Cadena de texto con la etiqueta para hombres.
#'   Por defecto "Hombres".
#'
#' @return Un tibble con columnas: Edad (grupo de edad, factor), Sexo
#'   (categoria de sexo, factor), Poblacion (poblacion absoluta,
#'   numerico), Porcentajes (porcentaje respecto al total, numerico),
#'   Pob (poblacion original antes de aplicar signo). Los valores de
#'   poblacion para hombres tienen signo negativo para su representacion
#'   en la piramide.
#'
#' @export
DemBas_datos_piramidePorc <- function(datosPiramide,
                                       GEdad_final = 100,
                                       etiq.hombre = "Hombres") {

  df2 = datosPiramide
  Edad2 = func_factor_to_numeric(df2$Edad)
  fct_Edad2 = DemBas_agrupar_variable(Edad2, metodo = 2, final = GEdad_final)
  #length(fct_Edad2)
  df2$Edad = fct_Edad2

  #df2$Poblacion = df2$value


  df2 = df2 %>%
    dplyr::group_by(Edad, Sexo) %>%
    dplyr::summarise( Poblacion2 = sum(Poblacion),.groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::rename(Poblacion = Poblacion2)

  df2$Poblacion[df2$Sexo==etiq.hombre] = - df2$Poblacion[df2$Sexo==etiq.hombre]
  #Gcadporcen = ""

  df2 = df2 %>%
    dplyr::mutate(
      Total = sum(abs(Poblacion)),
      Porcentajes = 100*round(Poblacion/Total,4)
    )

  df2 = df2 %>%
    dplyr::rename(
      Pob = Poblacion
    )
  df2$Poblacion = abs(df2$Pob)

  df2 = df2 |>
    dplyr::select(Edad, Sexo, Poblacion, Porcentajes, Pob)

  return(df2)

}

# datosPiramide = pop
# datosPiramide$Poblacion = pop$value
# names(datosPiramide)
# datosPiramide = datosPiramide[,c(1,2,4)]
# names(datosPiramide)
# pop3b = DemBas_datos_piramidePorc(datosPiramide,
#                                 GEdad_final = 100,
#                                 etiq.hombre = "Hombres")
# head(pop3b)


# > names(pop3)
# [1] "Edad"        "Sexo"        "Pob"         "Total"       "Porcentajes"
# [6] "Poblacion"
# > head(pop3)
# # A tibble: 6 × 6
# Edad  Sexo       Pob   Total Porcentajes Poblacion
# <fct> <fct>    <dbl>   <dbl>       <dbl>     <dbl>
#   1 0     Hombres  -7626 1873932       -0.41     -0.41
# 2 0     Mujeres   7172 1873932        0.38      0.38
# 3 1-4   Hombres -36685 1873932       -1.96     -1.96
# 4 1-4   Mujeres  34468 1873932        1.84      1.84
# 5 5-9   Hombres -53213 1873932       -2.84     -2.84
# 6 5-9   Mujeres  50372 1873932        2.69      2.69

# Necesarias: Edad, Sexo, Poblacion   (NO necesita: Porcentajes)

#pop3red = pop3[,c(1,2,5)]

#' Crea pirámide poblacional con porcentajes y etiquetas
#'
#' Genera una pirámide de población donde las barras representan
#' porcentajes de la población total. Incluye etiquetas con los
#' valores numéricos en cada barra y opciones para añadir segmentos
#' que separen grupos de edad (jóvenes, adultos, mayores).
#'
#' @param datosPiramide data.frame o tibble con columnas:
#'   \describe{
#'     \item{Edad}{Variable categórica o numérica con las edades}
#'     \item{Sexo}{Variable categórica ("Hombres" o "Mujeres")}
#'     \item{Poblacion}{Variable numérica con datos de población (absolutos)}
#'   }
#' @param Gtitulo Cadena de texto con el título del gráfico.
#'   Por defecto "Pirámide Población".
#' @param Gsubtitulo Cadena de texto con el subtítulo del gráfico.
#'   Por defecto "Año 2020".
#' @param Gtitulo.X Cadena de texto con la etiqueta del eje X.
#'   Por defecto "Porcentajes".
#' @param GHombresEtiq Cadena de texto con la etiqueta para hombres.
#'   Por defecto "Hombres".
#' @param GMujeresEtiq Cadena de texto con la etiqueta para mujeres.
#'   Por defecto "Mujeres".
#' @param Gedadfinal Valor numérico con la edad máxima final para
#'   agrupar. Por defecto 100.
#' @param Gext_izq Valor numérico con el límite izquierdo de la escala
#'   del eje Y (debe ser negativo). Por defecto -4.5.
#' @param Gext_der Valor numérico con el límite derecho de la escala
#'   del eje Y (debe ser positivo). Por defecto 4.5.
#' @param Glimite Valor numérico con el umbral mínimo para mostrar
#'   etiquetas. Por defecto 0.5.
#' @param Gsizeletra Valor numérico para el tamaño de la letra de
#'   las etiquetas. Por defecto 2.5.
#' @param GSegmentos Valor lógico. Si TRUE, añade segmentos verticales
#'   para separar grupos de edad. Por defecto TRUE.
#' @param Gguardar Valor lógico. Si TRUE, guarda el gráfico en un archivo.
#'   Por defecto FALSE.
#' @param Garchivo Cadena de texto con el nombre del archivo para guardar.
#'   Por defecto "piramide.png".
#' @param Gwidth Valor numérico con el ancho de la imagen en pulgadas.
#'   Por defecto 12.
#' @param Gheight Valor numérico con el alto de la imagen en pulgadas.
#'   Por defecto 10.
#'
#' @return Objeto ggplot2 con la pirámide poblacional.
#'
#' @examples
#' \dontrun{
#' load(file = system.file("examples/04003px.RData",
#'                         package = "DemographyBasic"))
#'
#' datosPiramide <- datos |>
#'   dplyr::filter(Ano == 2020,
#'                 Sexo %in% c("Mujeres", "Hombres"),
#'                 Edad != "TOTAL",
#'                 CCAA.Prov == "Sevilla",
#'                 Espanoles.Extranjeros == "Españoles") |>
#'   dplyr::rename(Poblacion = value) |>
#'   dplyr::select(Edad, Sexo, Poblacion)
#'
#' DemBas_piramidePorc(datosPiramide,
#'                    Gtitulo = "Pirámide Población de Sevilla",
#'                    Gsubtitulo = "Año 2020 (españoles)",
#'                    GSegmentos = FALSE)
#' }
#'
#' @export
DemBas_piramidePorc <- function(datosPiramide,
                               Gtitulo = "Pirámide Población",
                               Gsubtitulo = "Año 2020",
                               Gtitulo.X = "Porcentajes",
                               GHombresEtiq="Hombres",
                               GMujeresEtiq="Mujeres",
                               Gedadfinal = 100,
                               Gext_izq = -4.5,
                               Gext_der = 4.5,
                               Glimite = 0.5,
                               Gsizeletra = 2.5,
                               GSegmentos = TRUE,
                               Gguardar = FALSE,
                               Garchivo = "piramide.png",
                               Gwidth = 12,
                               Gheight = 10
                               ) {

  #Glimite = 0.5

  pop3 = DemBas_datos_piramidePorc(datosPiramide,
                                 GEdad_final = Gedadfinal,
                                 etiq.hombre = GHombresEtiq)

  rango = range(pop3$Porcentajes)
  if (rango[1]< Gext_izq) {
    message(paste0("Aviso: use un valor del argumento Gext_izq menor que: ", rango[1]))
  }
  if (rango[2]> Gext_der) {
    message(paste0("Aviso: use un valor del argumento Gext_der mayor que: ", rango[2]))
  }
  e1m = pop3 %>%
    filter(Sexo == GMujeresEtiq) %>%
    filter(Porcentajes > Glimite) %>%
    pull(Edad)

  e1h = pop3 %>%
    filter(Sexo == GHombresEtiq) %>%
    filter(Porcentajes < (-Glimite)) %>%
    pull(Edad)

  edad_si2 = base::intersect(e1m,e1h)

  b1m = rep(F,length(nrow(pop3)))
  b1h = rep(F,length(nrow(pop3)))

  dif1 = pop3$Porcentajes[pop3$Sexo==GMujeresEtiq] - (-pop3$Porcentajes[pop3$Sexo==GHombresEtiq])

  b1m[pop3$Sexo==GMujeresEtiq] = dif1>=0
  b1h[pop3$Sexo==GHombresEtiq] = dif1<0
  b1m[is.na(b1m)] = F
  b1h[is.na(b1h)] = F

  bars <- ggplot(pop3, aes(x = Edad, y = Porcentajes, fill = fct_drop(Sexo))) +
    geom_bar(stat="identity",
             position="identity"
    ) +
    geom_hline(yintercept = 0, size = 1, colour="#ff0000") +
    labs(title=Gtitulo,
         subtitle = Gsubtitulo,
         y = Gtitulo.X,
         fill = "Sexo")

  bars1 = bars +
    coord_flip() +
    theme(
      panel.grid.major.x = element_line(color="#cb99cb"),
      panel.grid.major.y = element_line(color="#cb99cb"),
      axis.text=element_text(size=7)
    ) +
    scale_y_continuous(limits=c(Gext_izq,Gext_der),
                       breaks = seq(Gext_izq, Gext_der, by = 0.5),
                       labels = paste0(abs(seq(Gext_izq,Gext_der,by=0.5)),"%")
    ) +
    scale_fill_manual(values=c("#5BB4E5", "#DE61D8")) +  #  blue, goldenrod2
    geom_label(aes(x = Edad,
                   y = ifelse(Porcentajes> (Glimite), 0.2, Porcentajes ),
                   label = ifelse(Sexo==GMujeresEtiq,
                                  paste0(sprintf("%.2f",
                                                 round(abs(Porcentajes),2)),"%"),"")
    ),
    hjust = 0,
    vjust = 0.5,
    colour = "black",#"white",
    fill = NA,
    linewidth = NA, # label.size = NA,
    #family="Helvetica",
    fontface = ifelse(b1m,"bold.italic","plain"),
    size = Gsizeletra) +
    geom_label(aes(x = Edad,  ## Hombres
                   y = ifelse(Porcentajes< (-Glimite), -0.2, Porcentajes ),
                   label = ifelse(Sexo==GHombresEtiq,
                                  paste0(sprintf("%.2f",
                                                 round(abs(Porcentajes),2)),"%"),"")
    ),
    hjust = 1,
    vjust = 0.5,
    colour = "black",#"white",
    fill = NA,
    linewidth = NA, # label.size = NA,
    #family="Helvetica",
    fontface = ifelse(b1h,"bold.italic","plain"),
    size = Gsizeletra)

  if (GSegmentos) {
    g_pirpob = bars1 +
      geom_segment(aes(x = 3.5, y = Gext_izq, xend = 3.5, yend = Gext_der),
                   colour = "#00ff00",
                   size=1.5) +
      geom_segment(aes(x = 13.5, y = Gext_izq, xend = 13.5, yend = Gext_der),
                   colour = "#00ff00",
                   size=1.5)

  } else {
    g_pirpob = bars1
  }


  if (Gguardar) {
    ggplot2::ggsave(Garchivo, g_pirpob, width = Gwidth, height = Gheight)
  }

  g_pirpob



}


# funciones para pirámides con información de generaciones y más datos -------



#' Calcula los años de nacimiento (generaciones) a partir de grupos de edad
#'
#' Toma un grupo de edad (ej. "0-4", "5-9", "100+") y calcula el rango
#' de años de nacimiento (generaciones) correspondiente, utilizando un
#' año de referencia. Por ejemplo, si Año_ref=2020 y x="5-9", devuelve
#' "2010-2015".
#'
#' @param x Cadena de texto o factor con el grupo de edad (ej. "0-4",
#'   "5-9", "10-14", "100+").
#' @param Ano_ref Valor numérico con el año de referencia para calcular
#'   las generaciones. Por defecto 2020.
#'
#' @return Cadena de texto con el rango de años de nacimiento
#'   (ej. "2010-2015", "1915-"). Para grupos abiertos como "100+",
#'   devuelve algo como "1915-".
#'
#' @examples
#' DemBas_generaciones("5-9", 2020)
#' DemBas_generaciones("100+", 2020)
#'
#' @export
DemBas_generaciones <- function(x, Ano_ref = 2020) {
  tt1 = as.character(x)
  tt2 = unlist(stringr::str_split(tt1,"-"))
  if (length(tt2)>1) {
    tt3 = Ano_ref - as.numeric(tt2)
    tt4 = paste0(tt3[2],"-",tt3[1])
  } else {
    tt3 = Ano_ref - as.numeric(stringr::str_replace_all(tt2,fixed("+"),""))
    tt4 = paste0(tt3[1],"-")
  }
  return(tt4)
}

#' Calcula las generaciones para un vector de grupos de edad
#'
#' Versión vectorizada de \code{\link{DemBas_generaciones}} que aplica
#' el cálculo a todos los elementos de un vector. Útil para añadir
#' etiquetas de generaciones a los ejes de pirámides poblacionales.
#'
#' @param x Vector de cadenas de texto o factores con los grupos de
#'   edad (ej. c("0-4", "5-9", "10-14", "100+")).
#' @param Ano_ref Valor numérico con el año de referencia para calcular
#'   las generaciones. Por defecto 2020.
#'
#' @return Vector de cadenas de texto con los rangos de años de
#'   nacimiento para cada grupo de edad.
#'
#' @seealso \code{\link{DemBas_generaciones}} para la función que
#'   procesa un solo valor.
#'
#' @examples
#' DemBas_v_generaciones(c("0-4", "5-9", "10-14", "100+"), 2020)
#'
#' @export
DemBas_v_generaciones <- function(x, Ano_ref = 2020) {
  return(sapply(x,DemBas_generaciones, Ano_ref = Ano_ref))
}


#' Crea pirámide poblacional con porcentajes, etiquetas y generaciones
#'
#' Genera una pirámide de población avanzada que incluye: porcentajes
#' de población por grupo de edad, etiquetas con valores numéricos,
#' líneas divisorias para separar jóvenes/adultos/mayores, y un eje
#' secundario con las generaciones (años de nacimiento) correspondientes.
#' También puede presentar un resumen estadístico con índices demográficos.
#'
#' @param pop3 data.frame o tibble con columnas:
#'   \describe{
#'     \item{Edad}{Variable categórica con grupos de edad (factor)}
#'     \item{Sexo}{Variable categórica ("Hombres" o "Mujeres")}
#'     \item{Porcentajes}{Variable numérica con los datos de
#'       población}
#'   }
#'   Esta función requiere que los datos ya estén procesados por
#'   \code{\link{DemBas_datos_piramidePorc}}.
#' @param Ano_ref Valor numérico con el año de referencia para calcular
#'   las generaciones. Por defecto 2020.
#' @param Gtitulo Cadena de texto con el título del gráfico.
#'   Por defecto "Pirámide Población".
#' @param Gsubtitulo Cadena de texto con el subtítulo del gráfico.
#'   Por defecto "Año 2020".
#' @param Gtitulo.X Cadena de texto con la etiqueta del eje X.
#'   Por defecto "Porcentajes".
#' @param GHombresEtiq Cadena de texto con la etiqueta para hombres.
#'   Por defecto "Hombres".
#' @param GMujeresEtiq Cadena de texto con la etiqueta para mujeres.
#'   Por defecto "Mujeres".
#' @param Gedadfinal Valor numérico con la edad máxima final para
#'   agrupar. Por defecto 100.
#' @param Gext_izq Valor numérico con el límite izquierdo de la escala
#'   del eje Y. Por defecto -4.5.
#' @param Gext_der Valor numérico con el límite derecho de la escala
#'   del eje Y. Por defecto 4.5.
#' @param Glimite Valor numérico con el umbral mínimo para mostrar
#'   etiquetas. Por defecto 0.5.
#' @param Gsizeletra Valor numérico para el tamaño de la letra de
#'   las etiquetas. Por defecto 2.5.
#' @param GpresentaResumen Valor lógico. Si TRUE, muestra un resumen
#'   estadístico en el pie del gráfico con información sobre porcentajes
#'   por sexo, grupos de edad e índice de envejecimiento. Por defecto TRUE.
#' @param GSegmentos Valor lógico. Si TRUE, añade segmentos verticales
#'   para separar grupos de edad. Por defecto TRUE.
#' @param GHombresColor Cadena de texto con el color para hombres.
#'   Por defecto "#5BB4E5" (azul).
#' @param GMujeresColor Cadena de texto con el color para mujeres.
#'   Por defecto "#DE61D8" (rosa).
#' @param GSegmentosColor Cadena de texto con el color de los segmentos.
#'   Por defecto "#00ff00" (verde).
#' @param Gguardar Valor lógico. Si TRUE, guarda el gráfico en un archivo.
#'   Por defecto FALSE.
#' @param Garchivo Cadena de texto con el nombre del archivo para guardar.
#'   Por defecto "piramide.png".
#' @param Gwidth Valor numérico con el ancho de la imagen en pulgadas.
#'   Por defecto 12.
#' @param Gheight Valor numérico con el alto de la imagen en pulgadas.
#'   Por defecto 10.
#'
#' @return Objeto ggplot2 con la pirámide poblacional avanzada.
#'
#' @references
#'   Para información sobre índices demográficos y estructura de la
#'   población, consulte manuales de demografía técnica.
#'
#' @examples
#' \dontrun{
#' load(file = system.file("examples/04003px.RData",
#'                         package = "DemographyBasic"))
#'
#' datosPiramide <- datos |>
#'   dplyr::filter(Ano == 2020,
#'                 Sexo %in% c("Mujeres", "Hombres"),
#'                 Edad != "TOTAL",
#'                 CCAA.Prov == "Sevilla",
#'                 Espanoles.Extranjeros == "Españoles") |>
#'   dplyr::rename(Poblacion = value) |>
#'   dplyr::select(Edad, Sexo, Poblacion)
#'
#' g_pir1 <- DemBas_piramidePorc_Generaciones(datosPiramide,
#'                                           Gtitulo = "Pirámide Población de Sevilla",
#'                                           Gsubtitulo = "Año 2020 (españoles)")
#' g_pir1
#'
#' g_pir2 <- DemBas_piramidePorc_Generaciones(datosPiramide,
#'                                           Gtitulo = "Pirámide Población de Sevilla",
#'                                           Gsubtitulo = "Año 2020 (españoles)",
#'                                           GSegmentos = FALSE,
#'                                           GpresentaResumen = FALSE)
#' g_pir2
#' }
#'
#' @export
DemBas_piramidePorc_Generaciones_ant <- function(
    pop3,
    Ano_ref = 2020,
    Gtitulo = "Pirámide Población",
    Gsubtitulo = "Año 2020",
    Gtitulo.X = "Porcentajes",
    GHombresEtiq = "Hombres",
    GMujeresEtiq = "Mujeres",
    Gedadfinal = 100,
    Gext_izq = -4.5,
    Gext_der = 4.5,
    Glimite = 0.5,
    Gsizeletra = 2.5,
    GpresentaResumen = TRUE,
    GSegmentos = TRUE,
    GHombresColor = "#5BB4E5",
    GMujeresColor = "#DE61D8",
    GSegmentosColor = "#00ff00",
    Gguardar = FALSE,
    Garchivo = "piramide.png",
    Gwidth = 12,
    Gheight = 10) {

  #Glimite = 0.5

  e1m = pop3 %>%
    filter(Sexo == GMujeresEtiq) %>%
    filter(Porcentajes > Glimite) %>%
    pull(Edad)

  e1h = pop3 %>%
    filter(Sexo == GHombresEtiq) %>%
    filter(Porcentajes < (-Glimite)) %>%
    pull(Edad)

  edad_si2 = base::intersect(e1m,e1h)

  b1m = rep(F,length(nrow(pop3)))
  b1h = rep(F,length(nrow(pop3)))


  dif1 = pop3$Porcentajes[pop3$Sexo==GMujeresEtiq] - (-pop3$Porcentajes[pop3$Sexo==GHombresEtiq])
  b1m[pop3$Sexo==GMujeresEtiq] = dif1>=0
  b1h[pop3$Sexo==GHombresEtiq] = dif1<0
  b1m[is.na(b1m)] = F
  b1h[is.na(b1h)] = F

  Est_PorSexo = pop3 %>%
    dplyr::group_by(Sexo) %>%
    dplyr::summarise(PorcentajesSexo = abs(sum(Porcentajes))) %>%
    pull(PorcentajesSexo)

  pop3 = pop3 %>%
    mutate(
      GGEdad = case_when(
        Edad %in% levels(pop3$Edad)[1:3] ~ "Jóvenes",
        Edad %in% levels(pop3$Edad)[4:13] ~ "Adultos",
        TRUE ~ "Mayores"
      )
    )



  temp1 = pop3 %>%
    dplyr::group_by(GGEdad) %>%
    dplyr::summarise(PorcentajesGGEdad = sum(abs(Porcentajes)), .groups = "keep" )

  Est_PorGGEdad = temp1 %>%
    pull(PorcentajesGGEdad)

  names(Est_PorGGEdad) = temp1 %>%
    pull(GGEdad)

  Est_PorGGEdadSexo = pop3 %>%
    dplyr::group_by(GGEdad,Sexo) %>%
    dplyr::summarise(PorcentajesGGEdad = sum(abs(Porcentajes)), .groups = "keep")

  t_Est_PorGGEdadSexo = glue::glue("Hombres-Jóvenes: {Est_PorGGEdadSexo[3,3]}%, Mujeres-Jóvenes: {Est_PorGGEdadSexo[4,3]}%, Hombres-Adultos: {Est_PorGGEdadSexo[1,3]}%, Mujeres-Adultas: {Est_PorGGEdadSexo[2,3]}%,
                                 Hombres-Mayores: {Est_PorGGEdadSexo[5,3]}%, Mujeres-Mayores: {Est_PorGGEdadSexo[6,3]}%")

  if (GpresentaResumen) {
    ResumenEstadistico = paste0("Porcentajes: Hombres=",Est_PorSexo[1],"%, Mujeres=",Est_PorSexo[2],"%,",
                                " Jóvenes=",Est_PorGGEdad["Jóvenes"], "%, Adultos=",Est_PorGGEdad["Adultos"],"%, Mayores=",Est_PorGGEdad["Mayores"],"%.\n",
                                t_Est_PorGGEdadSexo,
                                ". Índice de Envejecimiento=", round((Est_PorGGEdad["Mayores"]/Est_PorGGEdad["Jóvenes"])*100,2),"%")
  } else {
    ResumenEstadistico = ""
  }

  bars <- ggplot(pop3, aes(x = Edad, y = Porcentajes, fill = fct_drop(Sexo))) +
    geom_bar(stat="identity",
             position="identity"
    ) +
    geom_hline(yintercept = 0, size = 1, colour="#ff0000") +
    labs(title=Gtitulo,
         subtitle = Gsubtitulo,
         caption = ResumenEstadistico,
         y = "Porcentajes de población",
         x = "Grupos de Edad cumplida",
         fill = "Sexo"
    ) +
    guides(
      y.sec = ggh4x::guide_axis_manual(
        labels = ~ paste0(DemBas_v_generaciones(.x, Ano_ref = 2020)),
        title = "Generaciones Grupos de Edad"
      )
    )
  bars +
    coord_flip() +
    theme(
      #legend.position="none",
      panel.grid.major.x = element_line(color="#cb99cb"),
      panel.grid.major.y = element_line(color="#cb99cb")
    ) +
    scale_y_continuous(limits=c(-4.5,4.5),
                       breaks = seq(-4.5, 4.5, by = 0.5),
                       labels = paste0(abs(seq(-4.5,4.5,by=0.5)),"%")
    ) +
    scale_fill_manual(values=c(GHombresColor, GMujeresColor)) +  #  blue, goldenrod2
    geom_label(aes(x = Edad, ## Mujeres
                   y = ifelse(Porcentajes> (Glimite), 0.2, Porcentajes ),
                   label = ifelse(Sexo==GMujeresEtiq,
                                  paste0(sprintf("%.2f",round(abs(Porcentajes),2)),"%"),"")
    ),
    hjust = 0,
    vjust = 0.5,
    colour = "black",#"white",
    fill = NA,
    linewidth = NA, # label.size = NA,
    family="Helvetica",
    fontface = ifelse(b1m,"bold.italic","plain"),
    size = 4.5) +
    geom_label(aes(x = Edad,  ## Hombres
                   y = ifelse(Porcentajes< (-Glimite), -0.2, Porcentajes ),
                   label = ifelse(Sexo==GHombresEtiq,
                                  paste0(sprintf("%.2f",round(abs(Porcentajes),2)),"%"),"")
    ),
    hjust = 1,
    vjust = 0.5,
    colour = "black",#"white",
    fill = NA,
    linewidth = NA, # label.size = NA,
    family="Helvetica",
    fontface = ifelse(b1h,"bold.italic","plain"),
    size = 4.5) +
    geom_segment(aes(x = 3.5, y = -4.5, xend = 3.5, yend = 4.5),
                 colour = GSegmentosColor,
                 linewidth=1.5) +
    geom_segment(aes(x = 13.5, y = -4.5, xend = 13.5, yend = 4.5),
                 colour = GSegmentosColor,
                 linewidth=1.5) -> g_pirpob

  g_pirpob

}

# load(file = "pop3.RData")
# g_pir3gen = DemBas_piramidePorc_Generaciones_ant(pop3,GpresentaResumen=F)


# Función mejorada sin avisos de deprecación a continuación sin "_ant"
# Cambios realizados:
# 1. size = 1 en geom_hline() cambiado a linewidth = 1
# 2. guide_axis_manual() (ggh4x, deprecated) sustituido por legendry::guide_axis_base()
#    con key_manual() para mostrar correctamente las generaciones en el eje derecho
# 3. label.size = NA en geom_label() cambiado a linewidth = NA
# Nota: legendry es necesario para mapear etiquetas discretas de edad al eje Y secundario
#       (derecha del gráfico) tras coord_flip(). No hay equivalente en ggplot2 base ni ggh4x.



#' Crea pirámide poblacional con porcentajes, etiquetas y generaciones
#'
#' Genera una pirámide de población avanzada que incluye: porcentajes
#' de población por grupo de edad, etiquetas con valores numéricos,
#' líneas divisorias para separar jóvenes/adultos/mayores, y un eje
#' secundario con las generaciones (años de nacimiento) correspondientes.
#' También puede presentar un resumen estadístico con índices demográficos.
#'
#' @param pop3 data.frame o tibble con columnas:
#'   \describe{
#'     \item{Edad}{Variable categórica con grupos de edad (factor)}
#'     \item{Sexo}{Variable categórica ("Hombres" o "Mujeres")}
#'     \item{Porcentajes}{Variable numérica con los datos de
#'       población}
#'   }
#'   Esta función requiere que los datos ya estén procesados por
#'   \code{\link{DemBas_datos_piramidePorc}}.
#' @param Ano_ref Valor numérico con el año de referencia para calcular
#'   las generaciones. Por defecto 2020.
#' @param Gtitulo Cadena de texto con el título del gráfico.
#'   Por defecto "Pirámide Población".
#' @param Gsubtitulo Cadena de texto con el subtítulo del gráfico.
#'   Por defecto "Año 2020".
#' @param Gtitulo.X Cadena de texto con la etiqueta del eje X.
#'   Por defecto "Porcentajes".
#' @param GHombresEtiq Cadena de texto con la etiqueta para hombres.
#'   Por defecto "Hombres".
#' @param GMujeresEtiq Cadena de texto con la etiqueta para mujeres.
#'   Por defecto "Mujeres".
#' @param Gedadfinal Valor numérico con la edad máxima final para
#'   agrupar. Por defecto 100.
#' @param Gext_izq Valor numérico con el límite izquierdo de la escala
#'   del eje Y. Por defecto -4.5.
#' @param Gext_der Valor numérico con el límite derecho de la escala
#'   del eje Y. Por defecto 4.5.
#' @param Glimite Valor numérico con el umbral mínimo para mostrar
#'   etiquetas. Por defecto 0.5.
#' @param Gsizeletra Valor numérico para el tamaño de la letra de
#'   las etiquetas. Por defecto 2.5.
#' @param GpresentaResumen Valor lógico. Si TRUE, muestra un resumen
#'   estadístico en el pie del gráfico con información sobre porcentajes
#'   por sexo, grupos de edad e índice de envejecimiento. Por defecto TRUE.
#' @param GSegmentos Valor lógico. Si TRUE, añade segmentos verticales
#'   para separar grupos de edad. Por defecto TRUE.
#' @param GHombresColor Cadena de texto con el color para hombres.
#'   Por defecto "#5BB4E5" (azul).
#' @param GMujeresColor Cadena de texto con el color para mujeres.
#'   Por defecto "#DE61D8" (rosa).
#' @param GSegmentosColor Cadena de texto con el color de los segmentos.
#'   Por defecto "#00ff00" (verde).
#' @param Gguardar Valor lógico. Si TRUE, guarda el gráfico en un archivo.
#'   Por defecto FALSE.
#' @param Garchivo Cadena de texto con el nombre del archivo para guardar.
#'   Por defecto "piramide.png".
#' @param Gwidth Valor numérico con el ancho de la imagen en pulgadas.
#'   Por defecto 12.
#' @param Gheight Valor numérico con el alto de la imagen en pulgadas.
#'   Por defecto 10.
#'
#' @return Objeto ggplot2 con la pirámide poblacional avanzada.
#'
#' @references
#'   Para información sobre índices demográficos y estructura de la
#'   población, consulte manuales de demografía técnica.
#'
#' @examples
#' \dontrun{
#' load(file = system.file("examples/04003px.RData",
#'                         package = "DemographyBasic"))
#'
#' datosPiramide <- datos |>
#'   dplyr::filter(Ano == 2020,
#'                 Sexo %in% c("Mujeres", "Hombres"),
#'                 Edad != "TOTAL",
#'                 CCAA.Prov == "Sevilla",
#'                 Espanoles.Extranjeros == "Españoles") |>
#'   dplyr::rename(Poblacion = value) |>
#'   dplyr::select(Edad, Sexo, Poblacion)
#'
#' g_pir1 <- DemBas_piramidePorc_Generaciones(datosPiramide,
#'                                           Gtitulo = "Pirámide Población de Sevilla",
#'                                           Gsubtitulo = "Año 2020 (españoles)")
#' g_pir1
#'
#' g_pir2 <- DemBas_piramidePorc_Generaciones(datosPiramide,
#'                                           Gtitulo = "Pirámide Población de Sevilla",
#'                                           Gsubtitulo = "Año 2020 (españoles)",
#'                                           GSegmentos = FALSE,
#'                                           GpresentaResumen = FALSE)
#' g_pir2
#' }
#'
#' @export
DemBas_piramidePorc_Generaciones <- function(
    pop3,
    Ano_ref = 2020,
    Gtitulo = "Pirámide Población",
    Gsubtitulo = "Año 2020",
    Gtitulo.X = "Porcentajes",
    GHombresEtiq = "Hombres",
    GMujeresEtiq = "Mujeres",
    Gedadfinal = 100,
    Gext_izq = -4.5,
    Gext_der = 4.5,
    Glimite = 0.5,
    Gsizeletra = 2.5,
    GpresentaResumen = TRUE,
    GSegmentos = TRUE,
    GHombresColor = "#5BB4E5",
    GMujeresColor = "#DE61D8",
    GSegmentosColor = "#00ff00",
    Gguardar = FALSE,
    Garchivo = "piramide.png",
    Gwidth = 12,
    Gheight = 10) {


  pop3 = DemBas_datos_piramidePorc(pop3,
                                   GEdad_final = Gedadfinal,
                                   etiq.hombre = GHombresEtiq)

  rango = range(pop3$Porcentajes)
  if (rango[1]< Gext_izq) {
    message(paste0("Aviso: use un valor del argumento Gext_izq menor que: ", rango[1]))
  }
  if (rango[2]> Gext_der) {
    message(paste0("Aviso: use un valor del argumento Gext_der mayor que: ", rango[2]))
  }

  e1m <- pop3 %>%
    dplyr::filter(Sexo == GMujeresEtiq) %>%
    dplyr::filter(Porcentajes > Glimite) %>%
    dplyr::pull(Edad)

  e1h <- pop3 %>%
    dplyr::filter(Sexo == GHombresEtiq) %>%
    dplyr::filter(Porcentajes < (-Glimite)) %>%
    dplyr::pull(Edad)

  edad_si2 <- base::intersect(e1m, e1h)
  b1m <- rep(F, length(nrow(pop3)))
  b1h <- rep(F, length(nrow(pop3)))

  dif1 <- pop3$Porcentajes[pop3$Sexo == GMujeresEtiq] -
    (-pop3$Porcentajes[pop3$Sexo == GHombresEtiq])

  b1m[pop3$Sexo == GMujeresEtiq] <- dif1 >= 0
  b1h[pop3$Sexo == GHombresEtiq] <- dif1 < 0

  b1m[is.na(b1m)] <- F
  b1h[is.na(b1h)] <- F

  Est_PorSexo <- pop3 %>%
    dplyr::group_by(Sexo) %>%
    dplyr::summarise(PorcentajesSexo = abs(sum(Porcentajes))) %>%
    dplyr::pull(PorcentajesSexo)

  pop3 <- pop3 %>%
    dplyr::mutate(GGEdad = dplyr::case_when(
      Edad %in% levels(pop3$Edad)[1:3] ~ "Jóvenes",
      Edad %in% levels(pop3$Edad)[4:13] ~ "Adultos",
      TRUE ~ "Mayores"
    ))

  temp1 <- pop3 %>%
    dplyr::group_by(GGEdad) %>%
    dplyr::summarise(PorcentajesGGEdad = sum(abs(Porcentajes)), .groups = "keep")

  Est_PorGGEdad <- temp1 %>% dplyr::pull(PorcentajesGGEdad)
  names(Est_PorGGEdad) <- temp1 %>% dplyr::pull(GGEdad)

  Est_PorGGEdadSexo <- pop3 %>%
    dplyr::group_by(GGEdad, Sexo) %>%
    dplyr::summarise(PorcentajesGGEdad = sum(abs(Porcentajes)), .groups = "keep")

  if (GpresentaResumen) {
    ResumenEstadistico <- paste0(
      "Porcentajes: Hombres=", Est_PorSexo[1], "%, Mujeres=", Est_PorSexo[2], "%,",
      " Jóvenes=", Est_PorGGEdad["Jóvenes"], "%, Adultos=", Est_PorGGEdad["Adultos"],
      "%, Mayores=", Est_PorGGEdad["Mayores"], "%.",
      "\nHombres-Jóvenes=", round(Est_PorGGEdadSexo[3,3], 2), "%, Mujeres-Jóvenes=", round(Est_PorGGEdadSexo[4,3], 2),
      "%, Hombres-Adultos=", round(Est_PorGGEdadSexo[1,3], 2), "%, Mujeres-Adultas=", round(Est_PorGGEdadSexo[2,3], 2), "%.",
      "\nHombres-Mayores=", round(Est_PorGGEdadSexo[5,3], 2), "%, Mujeres-Mayores=", round(Est_PorGGEdadSexo[6,3], 2),
      "%. Índice de Envejecimiento=",
      round((Est_PorGGEdad["Mayores"] / Est_PorGGEdad["Jóvenes"]) * 100, 2), "%"
    )
  } else {
    ResumenEstadistico <- ""
  }
  gen_breaks <- levels(pop3$Edad)
  gen_labels <- DemographyBasic::DemBas_v_generaciones(gen_breaks, Ano_ref = Ano_ref)
  gen_labels <- sub("^(\\d+)-$", "-\\1", gen_labels)

  bars <- ggplot2::ggplot(pop3, ggplot2::aes(x = Edad, y = Porcentajes,
                                             fill = forcats::fct_drop(Sexo))) +
    ggplot2::geom_bar(stat = "identity", position = "identity") +
    ggplot2::geom_hline(yintercept = 0, linewidth = 1, colour = "#ff0000") +
    ggplot2::labs(
      title = Gtitulo,
      subtitle = Gsubtitulo,
      caption = ResumenEstadistico,
      y = "Porcentajes de población",
      x = "Grupos de Edad cumplida",
      fill = "Sexo"
    ) +
    ggplot2::guides(
      y.sec = legendry::guide_axis_base(
        key = legendry::key_manual(gen_breaks, label = gen_labels),
        title = "Generaciones Grupos de Edad"
      )
    )

  g_pirpob <- bars +
    ggplot2::scale_y_continuous(
      limits = c(Gext_izq, Gext_der),
      breaks = seq(Gext_izq, Gext_der, by = 0.5),
      labels = paste0(abs(seq(Gext_izq, Gext_der, by = 0.5)), "%")
    ) +
    ggplot2::scale_fill_manual(values = c(GHombresColor, GMujeresColor)) +
    ggplot2::coord_flip() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = "#cb99cb"),
      panel.grid.major.y = ggplot2::element_line(color = "#cb99cb")
    ) +
    ggplot2::geom_label(
      ggplot2::aes(
        x = Edad,
        y = ifelse(Porcentajes > (Glimite), 0.2, Porcentajes),
        label = ifelse(
          Sexo == GMujeresEtiq,
          paste0(sprintf("%.2f", round(abs(Porcentajes), 2)), "%"),
          ""
        )
      ),
      hjust = 0, vjust = 0.5, colour = "black", fill = NA, linewidth = NA,
      family = "Helvetica",
      fontface = ifelse(b1m, "bold.italic", "plain"),
      size = 4.5
    ) +
    ggplot2::geom_label(
      ggplot2::aes(
        x = Edad,
        y = ifelse(Porcentajes < (-Glimite), -0.2, Porcentajes),
        label = ifelse(
          Sexo == GHombresEtiq,
          paste0(sprintf("%.2f", round(abs(Porcentajes), 2)), "%"),
          ""
        )
      ),
      hjust = 1, vjust = 0.5, colour = "black", fill = NA, linewidth = NA,
      family = "Helvetica",
      fontface = ifelse(b1h, "bold.italic", "plain"),
      size = 4.5
    )


  if (GSegmentos) {
    g_pirpob = g_pirpob +
      geom_segment(aes(x = 3.5, y = Gext_izq, xend = 3.5, yend = Gext_der),
                   colour = GSegmentosColor,
                   linewidth=1.5) +
      geom_segment(aes(x = 13.5, y = Gext_izq, xend = 13.5, yend = Gext_der),
                   colour = GSegmentosColor,
                   linewidth=1.5)

  }


  if (Gguardar) {
    ggplot2::ggsave(Garchivo, g_pirpob, width = Gwidth, height = Gheight)
  }

  g_pirpob
}

#pak::pak("RConsortium/S7")
#pak::pak("r-lib/gtable")
#pak::pak("r-lib/scales")
#pak::pak("tidyverse/ggplot2")  # para ggplot2 >= 4.0.0 para legendry
#pak::pak("teunbrand/legendry")
#load(file = "pop3.RData")
#load(file = system.file("examples/pop3.RData", package = "DemographyBasic"))
#g_pir3gen = DemBas_piramidePorc_Generaciones2(pop3,GpresentaResumen=F)
#g_pir3gen = DemBas_piramidePorc_Generaciones2(pop3,GpresentaResumen=T)
#g_pir3gen = DemBas_piramidePorc_Generaciones(pop3,GpresentaResumen=F)
#g_pir3gen = DemBas_piramidePorc_Generaciones(pop3,GpresentaResumen=T)

