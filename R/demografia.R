# funciones de Demografía

library(stringr)

# Tasas calculadas y multiplicadas por mil


#' @title DemBas_dem_TasaBruta_t
#'
#' @param NumEventos_t
#' @param PobMedia_t
#'
#' @returns
#' @export
DemBas_dem_TasaBruta_t = function(NumEventos_t,PobMedia_t) {
  return((NumEventos_t/PobMedia_t)*1000)
}

#' @title DemBas_dem_TasaEspecifica_t
#'
#' @param NumEventos_t
#' @param PobMedia_Referida_t
#'
#' @returns
#' @export
DemBas_dem_TasaGeneral_t = function(NumEventos_t,PobMedia_Referida_t) {
  return((NumEventos_t/PobMedia_Referida_t)*1000)
}

#' @title DemBas_dem_TasaEspecifica_t
#'
#' @param vNumEventos_x_t
#' @param vPobMedia_x_t
#'
#' @returns
#' @export
DemBas_dem_TasaEspecifica_t = function(vNumEventos_x_t,vPobMedia_x_t) {
  return((vNumEventos_x_t/vPobMedia_x_t)*1000)
}

#' @title DemBas_dem_TasaEspecificaEdad_t
#'
#' @param vTasasEspecificas_x_t
#'
#' @returns
#' @export
DemBas_dem_ISinteticoCoyuntural_t = function(vTasasEspecificas_x_t) {
  return(sum(vTasasEspecificas_x_t,na.rm = T)/1000)
}

#' @title DemBas_dem_EdadMedia_t
#'
#' @param vTasasEspecificas_x_t
#' @param Edad.marcas
#'
#' @returns
#' @export
DemBas_dem_EdadMedia_t = function(vTasasEspecificas_x_t,Edad.marcas) {
  numer = sum(vTasasEspecificas_x_t * Edad.marcas,na.rm = T)/1000
  denom = DemBas_dem_ISinteticoCoyuntural_t(vTasasEspecificas_x_t)
  return(numer/denom)
}



###
#### OTRAS UTILIDADES
###


#' @title DemBas_elimina_codigo_CCAA
#'
#' @param columna
#'
#' @returns
#' @export
DemBas_elimina_codigo_CCAA = function(columna) {

  pattern <- "\\d\\d\\s"
  reemplazo = ""
  nombre = as.character(stringr::str_replace_all(columna,pattern,reemplazo))
  nombre = stringr::str_trim(nombre)
  return(nombre)
}



#' @title DemBas_extrae_codigo_CCAA
#'
#' @param columna
#'
#' @returns
#' @export
DemBas_extrae_codigo_CCAA = function(columna) {
  pattern <- "\\d\\d"
  codigo = as.character(stringr::str_extract(columna,pattern))
  codigo = stringr::str_trim(codigo)
  #codigo[1] = "00"
  return(codigo)
}


#' @title DemBas_extrae_notienen_codigos
#'
#' @param columna
#' @param columnacodigos
#'
#' @returns
#' @export
DemBas_extrae_notienen_codigos = function(columna,columnacodigos) {
  ind = which(is.na(columnacodigos))
  salida = unique(columna[ind])
  return(salida)
}


#' @title DemBas_asigna_codigos_CCAA
#'
#' @param columna
#' @param columna_codigos
#' @param etiqueta
#' @param nuevocodigo
#'
#' @returns
#' @export
DemBas_asigna_codigos_CCAA = function(columna,columna_codigos,etiqueta,nuevocodigo) {
  ind = which(columna==etiqueta)
  columna_codigos[ind] = nuevocodigo
  return(columna_codigos)
}

#' @title DemBas_extrae_Num_Edad
#'
#' @param columna
#'
#' @returns
#' @export
DemBas_extrae_Num_Edad = function(columna) {
  pattern <- "\\d+\\s"
  codigo = as.character(stringr::str_extract(columna,pattern))
  codigo = stringr::str_trim(codigo)
  #codigo[1] = "00"
  return(codigo)
}


# Estaban en funciones_piramides.R ------


#' @title DemBas_agrupar_variable
#'
#' @param variable
#' @param metodo
#' @param final
#' @param vbreaks
#' @param vlabels
#' @param labelfinal
#'
#' @returns
#' @export
DemBas_agrupar_variable = function(variable,metodo=1,final=85,
                                 vbreaks=NULL,vlabels=NULL,labelfinal=NULL) {

  if (is.null(labelfinal)) {
    lbfinal = paste0(final,"+")
  } else {
    lbfinal = labelfinal
  }

  if (is.null(vbreaks)) {
    if (metodo==1) {
      Agrup = cut(variable,breaks = c(0,1,seq(5,final,by=5),200),right = F,
                  labels = c("0","1-4",paste0(seq(5,final-5,by=5),"-",seq(9,final,by=5)),lbfinal))
      return(Agrup)
    }
    if (metodo==2) {
      Agrup = cut(variable,breaks = c(seq(0,final,by=5),200),right = F,
                  labels = c(paste0(seq(0,final-5,by=5),"-",seq(4,final,by=5)),lbfinal))
      return(Agrup)
    }
  } else {
    if (is.null(vlabels)) {
      Agrup = cut(variable,breaks = vbreaks,right = F)
    } else {
      Agrup = cut(variable,breaks = vbreaks,right = F,
                  labels = vlabels )
    }
    return(Agrup)
  }


}


####
####
####


#' @title DemBas_etiquetas_gruposEdad
#'
#' @param metodo
#' @param final
#' @param labelfinal
#'
#' @returns
#' @export
DemBas_etiquetas_gruposEdad = function(metodo=1,final=85,labelfinal=NULL) {

  if (is.null(labelfinal)) {
    lbfinal = paste0(final,"+")
  } else {
    lbfinal = labelfinal
  }

  if (metodo==1) {
    Agrup = c("0","1-4",paste0(seq(5,final-5,by=5),"-",seq(9,final,by=5)),lbfinal)
    return(Agrup)
  }
  if (metodo==2) {
    Agrup = c(paste0(seq(0,final-5,by=5),"-",seq(4,final,by=5)),lbfinal)
    return(Agrup)
  }

}




# Estaban en funciones_mapas.R ------
#
#' @title DemBas_extrae_codigo_provincia
#'
#' @param vprovincias
#'
#' @returns vector character
#' @export
DemBas_extrae_codigo_provincia = function(vprovincias) {

  resul <- sapply(vprovincias,
                  function(x) strsplit(x = as.character(x), split = " ")[[1]][1])
  return(resul)
}


#' @title DemBas_crea_colores_brewer
#' @description crea vector de colores con paleta brewer
#' @param cuantos
#' @param que_paletacolor
#'
#' @returns
#' @export
DemBas_crea_colores_brewer = function(cuantos,que_paletacolor=3) {
  paletas = c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges",
              "OrRd", "PuBu", "PuBuGn","PuRd", "Purples", "RdPu", "Reds",
              "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")
  if (is.character(que_paletacolor)) {
    cual = which(que_paletacolor %in% paletas)
    if (cual>0) {
      paletacolor = que_paletacolor
    } else {
      paletacolor = "BuPu"
    }
  } else {
    paletacolor = paletas[que_paletacolor]
  }
  colors <- brewer.pal(cuantos, paletacolor)
  return(colors)
}


#' @title DemBas_extrae_codigo_ccaa
#'
#' @param vCCAA
#' @param ConvierteCodSIANE
#'
#' @returns
#' @export
DemBas_extrae_codigo_ccaa = function(vCCAA,ConvierteCodSIANE=TRUE) {

  resul <- sapply(vCCAA,
                  function(x) strsplit(x = as.character(x), split = " ")[[1]][1])
  #is.character(indenv17ccaa$Codigo)
  if (ConvierteCodSIANE) {
    resul = as.numeric(resul)+60  # En SIANE: ccaa -> 60,61,...,79
  }
  return(resul)
}



# varEdad debe ser de tipo: numeric o integer
#' @title DemBas_anade_GEdad5
#' @description Añade una variable de grupos quinquenales de edad
#' @param datos data.frame o tibble con una variable de edad
#' @param varEdad debe ser de tipo: numeric o integer
#'
#' @return
#' @export
#'
#' @examples
#' datos = DemBas_read_px(system.file("examples/9663.px", package = "DemographyBasic"))
#' datosPob = datos |>
#'   dplyr::filter(Periodo == "1 de enero de  2018",
#'                 Edad.simple != "Total",
#'                 Sexo != "Ambos sexos") |>
#'   dplyr::select(Sexo, Edad.simple, Poblacion = value)
#'
#' datosPob2 = datosPob |>
#'   dplyr::mutate(
#'     Edad = as.numeric(gsub("[años|año]", "", Edad.simple)),
#'     Poblacion = round(Poblacion, 0)
#'   )
#'
#' datosPob2_conGruposEdad = DemBas_anade_GEdad5(datosPob2, Edad)
#' head(datosPob2_conGruposEdad,15)
#'
#'
#' datosPirAgru = DemBas_anade_GEdad5(datosPob2, Edad) |>
#'   dplyr::select(Sexo, Edad, Poblacion, GEdad5) |>
#'   dplyr::group_by(GEdad5, Sexo) |>
#'   dplyr::summarise(Poblacion = sum(Poblacion), .groups = "keep")
#'
#' head(datosPirAgru)
#'
DemBas_anade_GEdad5=function(datos, varEdad) {
  datosGE= datos |>
    dplyr::mutate(
      GEdad5 = dplyr::case_when(
        ({{varEdad}} >= 0) & ({{varEdad}} <= 4) ~ "  0-4",
        ({{varEdad}} >= 5) & ({{varEdad}} <= 9) ~ "  5-9",
        ({{varEdad}} >= 10) & ({{varEdad}} <= 14) ~ " 10-14",
        ({{varEdad}} >= 15) & ({{varEdad}} <= 19) ~ " 15-19",
        ({{varEdad}} >= 20) & ({{varEdad}} <= 24) ~ " 20-24",
        ({{varEdad}} >= 25) & ({{varEdad}} <= 29) ~ " 25-29",
        ({{varEdad}} >= 30) & ({{varEdad}} <= 34) ~ " 30-34",
        ({{varEdad}} >= 35) & ({{varEdad}} <= 39) ~ " 35-39",
        ({{varEdad}} >= 40) & ({{varEdad}} <= 44) ~ " 40-44",
        ({{varEdad}} >= 45) & ({{varEdad}} <= 49) ~ " 45-49",
        ({{varEdad}} >= 50) & ({{varEdad}} <= 54) ~ " 50-54",
        ({{varEdad}} >= 55) & ({{varEdad}} <= 59) ~ " 55-59",
        ({{varEdad}} >= 60) & ({{varEdad}} <= 64) ~ " 60-64",
        ({{varEdad}} >= 65) & ({{varEdad}} <= 69) ~ " 65-69",
        ({{varEdad}} >= 70) & ({{varEdad}} <= 74) ~ " 70-74",
        ({{varEdad}} >= 75) & ({{varEdad}} <= 79) ~ " 75-79",
        ({{varEdad}} >= 80) & ({{varEdad}} <= 84) ~ " 80-84",
        ({{varEdad}} >= 85) & ({{varEdad}} <= 89) ~ " 85-89",
        ({{varEdad}} >= 90) & ({{varEdad}} <= 94) ~ " 90-94",
        ({{varEdad}} >= 95) & ({{varEdad}} <= 99) ~ " 95-99",
        ({{varEdad}} >= 100) ~ "100+"
      ),
      GEdad5Num = dplyr::case_when(
        ({{varEdad}} >= 0) & ({{varEdad}} <= 4) ~ 0,
        ({{varEdad}} >= 5) & ({{varEdad}} <= 9) ~ 5,
        ({{varEdad}} >= 10) & ({{varEdad}} <= 14) ~ 10,
        ({{varEdad}} >= 15) & ({{varEdad}} <= 19) ~ 15,
        ({{varEdad}} >= 20) & ({{varEdad}} <= 24) ~ 20,
        ({{varEdad}} >= 25) & ({{varEdad}} <= 29) ~ 25,
        ({{varEdad}} >= 30) & ({{varEdad}} <= 34) ~ 30,
        ({{varEdad}} >= 35) & ({{varEdad}} <= 39) ~ 35,
        ({{varEdad}} >= 40) & ({{varEdad}} <= 44) ~ 40,
        ({{varEdad}} >= 45) & ({{varEdad}} <= 49) ~ 45,
        ({{varEdad}} >= 50) & ({{varEdad}} <= 54) ~ 50,
        ({{varEdad}} >= 55) & ({{varEdad}} <= 59) ~ 55,
        ({{varEdad}} >= 60) & ({{varEdad}} <= 64) ~ 60,
        ({{varEdad}} >= 65) & ({{varEdad}} <= 69) ~ 65,
        ({{varEdad}} >= 70) & ({{varEdad}} <= 74) ~ 70,
        ({{varEdad}} >= 75) & ({{varEdad}} <= 79) ~ 75,
        ({{varEdad}} >= 80) & ({{varEdad}} <= 84) ~ 80,
        ({{varEdad}} >= 85) & ({{varEdad}} <= 89) ~ 85,
        ({{varEdad}} >= 90) & ({{varEdad}} <= 94) ~ 90,
        ({{varEdad}} >= 95) & ({{varEdad}} <= 99) ~ 95,
        ({{varEdad}} >= 100) ~ 100
      )
    )
  return(datosGE)
}

#df01 = func_anade_GEdad5(df_Pob_sexo, Edad)


