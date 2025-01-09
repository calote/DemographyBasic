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



################
#### OTRAS UTILIDADES
################


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
