library(tidyverse)
#library(ggplot2)
library(ggthemes)
library(glue)

# func_piramide_ggplot2()
# func_piramides_enfrentadas_ggplot2()
# func_piramide_superpuestas_ggplot2()
# func_piramide_compuestasCateg_ggplot2()
# func_agrupar_variable()
# func_etiquetas_gruposEdad(metodo=1,final=85,labelfinal=NULL)

# func_piramide_ggplot2_linea()

###
###
###

# Los datos, df o tibble, deben tener tres columnas:
#   Edad (chr o fct) (simples o grupos edad),
#   Sexo (chr o fct) ("Hombre" o "Mujer"),
#   Poblacion (num) (absolutos)
#   Caso (chr o fct) (variable categórica para pirámides compuestasCateg)

#' @title DemBas_piramide_ggplot2
#' @description Función para crear pirámides poblacionales con ggplot2
#' @param datosPiramide data.frame o tibble, deben tener tres o cuatro columnas:
#'   Edad (chr o fct) (simples o grupos edad),
#'   Sexo (chr o fct) ("Hombre" o "Mujer"),
#'   Poblacion (num) (absolutos)
#'   Caso (chr o fct) (variable categórica para pirámides compuestasCateg)
#' @param porcentajes
#' @param etiquetas
#' @param etiquetas.size
#' @param UsaCaso
#' @param etiq.hombre
#' @param etiq.mujer
#' @param colorear
#' @param colores
#'
#' @returns ggplot2 graphics
#' @examples
#' dfej02a <- DemBas_read_px(system.file("examples/9663.px", package = "DemographyBasic"))
#' head(dfej02a)
#' tp1 = dfej02a %>%
#'   dplyr::filter(Periodo=="1 de enero de  2017",Edad.simple=="Total") %>%
#'   # hay dos espacios entre "de" y "2017"
#'   dplyr::select("Sexo","value")
#'
#' PV = round(tp1$value[tp1$Sexo=="Hombres"]/tp1$value[tp1$Sexo=="Ambos sexos"],
#'            4)*100
#' #### España a 1 de enero de 2017.
#'
#' x1 = as.character(DemBas_extrae_codigo_provincia(dfej02a$Edad.simple))
#' x1n = as.numeric(x1)
#'
#' x1ngr = DemBas_agrupar_variable(x1n,metodo=2,final=100)
#'
#' dfej02a$EdadGrupos = x1ngr
#'
#' tp3 = dfej02a %>%
#'   dplyr::filter( Periodo=="1 de enero de  2017",
#'                  !(Edad.simple %in% c("100 y más años","Total"))) %>%
#'   dplyr::group_by(Sexo,EdadGrupos) %>%
#'   dplyr::summarise(Poblacion = round(sum(value,na.rm=T),0), .groups = "keep")
#'
#'
#' dfPir2017 = dfej02a %>%
#'   dplyr::filter( Periodo=="1 de enero de  2017",
#'                  !(Sexo=="Ambos sexos"),
#'                  !(Edad.simple %in% c("100 y más años","Total"))) %>%
#'   dplyr::select(Edadchar=Edad.simple,
#'                 Sexo,
#'                 Poblacion = value)
#' dfPir2017$Edad = DemBas_extrae_codigo_provincia(dfPir2017$Edadchar)
#' dfPir2017$Edad = factor(dfPir2017$Edad,levels =unique(dfPir2017$Edad))
#' dfPir2017$Poblacion[is.na(dfPir2017$Poblacion)] = 0
#' head(dfPir2017)
#' DemBas_piramide_ggplot2(dfPir2017,
#'                       #etiquetas = T,etiquetas.size = 2,
#'                       etiq.hombre = "Hombres",etiq.mujer = "Mujeres") +
#'   labs(title = "Pirámide de Población de España en 2017") +
#'   scale_x_discrete(
#'     # si la variable edad fuera numeric debería usarse scale_x_continuous
#'     breaks = seq(0,105,by=5),
#'     labels = paste0(as.character(seq(0,105,by=5)), ""))


#' @export
DemBas_piramide_ggplot2 = function(datosPiramide,
                                   porcentajes=TRUE,
                                   etiquetas=FALSE,etiquetas.size=4,
                                   UsaCaso=FALSE,
                                   etiq.hombre="Hombre",etiq.mujer="Mujer",
                                   colorear="Sexo",colores=NULL,
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


####
####
####

#' @title DemBas_piramides_enfrentadas_ggplot2
#'
#' @param datosPiramide
#' @param porcentajes
#' @param etiquetas
#' @param etiquetas.size
#' @param UsaCaso
#' @param etiq.hombre
#' @param etiq.mujer
#' @param colorear
#' @param colores
#' @param nfilas
#' @param ncols
#'
#' @returns
#' @export
DemBas_piramides_enfrentadas_ggplot2 = function(datosPiramide,
                                              porcentajes=TRUE,
                                              etiquetas=FALSE,etiquetas.size=4,
                                              UsaCaso=TRUE,
                                              etiq.hombre="Hombre",etiq.mujer="Mujer",
                                              colorear="Sexo",colores=NULL,
                                              nfilas=NULL,ncols=NULL) {
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


#' @title DemBas_piramide_superpuestas_ggplot2
#'
#' @param datosPiramide
#' @param porcentajes
#' @param etiquetas
#' @param etiquetas.size
#' @param colores
#' @param transparente
#' @param alfa
#' @param bar.size
#' @param etiq.hombre
#' @param etiq.mujer
#'
#' @returns
#' @examples
#' # Ejecutar el código ejemplo de la función DemBas_piramide_ggplot2
#' dfPir2002 = dfej02a %>%
#'   dplyr::filter( Periodo=="1 de enero de  2002",
#'                  !(Sexo=="Ambos sexos"),
#'                  !(Edad.simple %in% c("100 y más años","Total"))) %>%
#'   dplyr::select(Edadchar=Edad.simple,
#'                 Sexo,
#'                 Poblacion = value)
#' dfPir2002$Edad = DemBas_extrae_codigo_provincia(dfPir2002$Edadchar)
#' dfPir2002$Edad = factor(dfPir2002$Edad,levels =unique(dfPir2002$Edad))
#' dfPir2002$Poblacion[is.na(dfPir2002$Poblacion)] = 0
#' dfPir2002y2017 = rbind(dfPir2002,dfPir2017)
#' dfPir2002y2017$Caso = c(rep(2002,nrow(dfPir2002)),rep(2017,nrow(dfPir2017)))
#' head(dfPir2002y2017)
#' DemBas_piramide_superpuestas_ggplot2(dfPir2002y2017,
#'                                    etiq.hombre = "Hombres",etiq.mujer = "Mujeres",
#'                                    transparente = T) +
#'   labs(title = "Pirámides de Población de España en 2002 y 2017 superpuestas") +
#'   scale_x_discrete(
#'     breaks = seq(0,105,by=5),
#'     labels = paste0(as.character(seq(0,105,by=5)), ""))
#' @export
DemBas_piramide_superpuestas_ggplot2 = function(datosPiramide,
                                                porcentajes=TRUE,
                                                etiquetas=FALSE,etiquetas.size=4,
                                                colores = NULL,
                                                transparente=FALSE,
                                                alfa=0.1,bar.size=1,
                                                etiq.hombre="Hombre",etiq.mujer="Mujer",
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


#' @title DemBas_piramide_compuestasCateg_ggplot2
#'
#' @param datosPiramide
#' @param porcentajes
#' @param etiquetas
#' @param etiquetas.size
#' @param colores
#' @param ordeninverso
#' @param alfa
#' @param bar.size
#' @param etiq.hombre
#' @param etiq.mujer
#'
#' @returns
#' @export
DemBas_piramide_compuestasCateg_ggplot2 = function(datosPiramide,
                                                   porcentajes=TRUE,
                                                   etiquetas=FALSE,etiquetas.size=4,
                                                   colores = NULL,ordeninverso=FALSE,
                                                   alfa=1,bar.size=1,
                                                   etiq.hombre="Hombre",etiq.mujer="Mujer") {
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

#' @title DemBas_piramide_ggplot2_linea
#'
#' @param datosPiramide
#' @param porcentajes
#' @param etiquetas
#' @param etiquetas.size
#' @param UsaCaso
#' @param etiq.hombre
#' @param etiq.mujer
#' @param colorear
#' @param colores
#'
#' @returns
#' @examples
#' # Ejecutar ejemplo de función DemBas_piramide_ggplot2()
#' DemBas_piramide_ggplot2_linea(dfPir2017,colorear = "Sexo",
#'                             etiq.hombre = "Hombres",etiq.mujer = "Mujeres") +
#'   labs(title = "Perfil de la Pirámide de Población de España en 2017") +
#'   scale_x_discrete(
#'     # si la variable edad fuera numeric debería usarse scale_x_continuous
#'     breaks = seq(0,105,by=5),
#'     labels = paste0(as.character(seq(0,105,by=5)), "")) +
#'   guides(colour="none")
#' @export
DemBas_piramide_ggplot2_linea = function(datosPiramide,
                                         porcentajes=TRUE,
                                         etiquetas=FALSE,etiquetas.size=4,
                                         UsaCaso=FALSE,
                                         etiq.hombre="Hombre",etiq.mujer="Mujer",
                                         colorear="Sexo",colores=NULL) {
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

func_factor_to_numeric = function(x) {
  return( as.numeric( as.character( x ) ) )
}

# datosPiramide
# columnas: Edad (fac,num, simples: 0,1,2,...), Sexo (factor), Poblacion (num)

#' @title DemBas_datos_piramidePorc
#'
#' @param datosPiramide data.frame o tibble con columnas: Edad (fac,num, simples: 0,1,2,...), Sexo (factor), Poblacion (num)
#' @param GEdad_final
#' @param etiq.hombre
#'
#' @returns
#' @export
DemBas_datos_piramidePorc = function(datosPiramide,
                                     GEdad_final = 100,
                                     etiq.hombre = "Hombres") {

  df2 = datosPiramide
  Edad2 = func_factor_to_numeric(df2$Edad)
  fct_Edad2 = func_agrupar_variable(Edad2, metodo = 2, final = GEdad_final)
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
# pop3b = func_datos_piramidePorc(datosPiramide,
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

#' @title DemBas_piramidePorc
#'
#' @param datosPiramide
#' @param Gtitulo
#' @param Gsubtitulo
#' @param Gtitulo.X
#' @param GHombresEtiq
#' @param GMujeresEtiq
#' @param Gedadfinal
#' @param Gext_izq
#' @param Gext_der
#' @param Glimite
#' @param Gsizeletra
#' @param GSegmentos
#'
#' @returns ggplot2 graphics de la pirámide de Población con porcentajes
#' @examples
#' load(file = system.file("examples/04003px.RData", package = "DemographyBasic"))
#'
#' ano_selec = 2020
#' Espanoles_Extranjeros = "Españoles"
#' CCAA_Prov = "Sevilla"
#'
#' datosPiramide =  datos |>
#'   dplyr::filter(Ano == ano_selec &
#'                   Sexo %in% c("Mujeres", "Hombres") &
#'                   Edad != "TOTAL" &
#'                   CCAA.Prov == CCAA_Prov &
#'                   Espanoles.Extranjeros == Espanoles_Extranjeros) |>
#'   dplyr::rename(Poblacion = value) |>
#'   dplyr::select(Edad, Sexo, Poblacion)
#' (g_pir1 = DemBas_piramidePorc(datosPiramide,
#'                             Gtitulo = "Pirámide Población de la provincia de Sevilla",
#'                             Gsubtitulo = "Año 2020  (españoles)",
#'                             GSegmentos = FALSE))
#'
#' @export
DemBas_piramidePorc = function(datosPiramide,
                               Gtitulo = "Pirámide Población de la provincia de Sevilla",
                               Gsubtitulo = "Año 2020",
                               Gtitulo.X = "Porcentajes",
                               GHombresEtiq="Hombres",
                               GMujeresEtiq="Mujeres",
                               Gedadfinal = 100,
                               Gext_izq = -4.5,
                               Gext_der = 4.5,
                               Glimite = 0.5,
                               Gsizeletra = 2.5,
                               GSegmentos = TRUE) {

  #Glimite = 0.5

  pop3 = func_datos_piramidePorc(datosPiramide,
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
    label.size = NA,
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
    label.size = NA,
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

  g_pirpob



}
