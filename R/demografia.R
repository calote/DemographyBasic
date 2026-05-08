# funciones de Demografía

library(stringr)

# Tasas calculadas y multiplicadas por mil


#' Calcula la tasa bruta de un evento demográfico
#'
#' Calcula la tasa bruta de mortalidad, natalidad u otro evento
#' demográfico por cada 1000 habitantes. La tasa bruta es el cociente
#' entre el número de eventos en un período y la población media
#' de ese período, multiplicado por 1000.
#'
#' @param NumEventos_t Valor numérico o vector con el número de eventos
#'   demográficos (defunciones, nacimientos, etc.) en el período t.
#' @param PobMedia_t Valor numérico o vector con la población media
#'   durante el período t.
#'
#' @return Vector numérico con las tasas brutas por 1000 habitantes.
#'
#' @examples
#' # Tasa bruta de mortalidad de España en 2020
#' DemBas_dem_TasaBruta_t(NumEventos_t = 492930, PobMedia_t = 47415750)
#'
#' @export
DemBas_dem_TasaBruta_t <- function(NumEventos_t, PobMedia_t) {
  return((NumEventos_t / PobMedia_t) * 1000)
}


#' Calcula la tasa general de un evento demográfico
#'
#' Calcula la tasa general de mortalidad u otro evento demográfico
#' por cada 1000 habitantes de la población de referencia. A diferencia
#' de la tasa bruta, la tasa general se calcula sobre una población
#' específica (ej. población en edad fértil para tasas de natalidad).
#'
#' @param NumEventos_t Valor numérico o vector con el número de eventos
#'   demográficos en el período t.
#' @param PobMedia_Referida_t Valor numérico o vector con la población
#'   media de referencia para el cálculo (ej. mujeres de 15 a 49 años).
#'
#' @return Vector numérico con las tasas generales por 1000 habitantes.
#'
#' @examples
#' # Tasa general de natalidad
#' DemBas_dem_TasaGeneral_t(NumEventos_t = 350000, PobMedia_Referida_t = 13000000)
#'
#' @export
DemBas_dem_TasaGeneral_t <- function(NumEventos_t, PobMedia_Referida_t) {
  return((NumEventos_t / PobMedia_Referida_t) * 1000)
}


#' Calcula tasas específicas de mortalidad (por edad)
#'
#' Calcula tasas específicas para cada grupo de edad, expresadas
#' por cada 1000 habitantes. Las tasas específicas permiten comparar
#' estructuras demográficas controlando por la distribución por edades.
#'
#' @param vNumEventos_x_t Vector numérico con el número de eventos
#'   (ej. defunciones) por grupo de edad x en el período t.
#' @param vPobMedia_x_t Vector numérico con la población media por
#'   grupo de edad x en el período t. Debe tener la misma longitud
#'   que vNumEventos_x_t.
#'
#' @return Vector numérico con las tasas específicas por 1000 habitantes
#'   para cada grupo de edad.
#'
#' @examples
#' # Tasas específicas de mortalidad por edades
#' eventos <- c(1200, 800, 600, 400, 300, 200, 150, 100, 80, 70)
#' poblacion <- c(50000, 60000, 70000, 80000, 90000, 95000, 100000, 85000, 70000, 60000)
#' tasas <- DemBas_dem_TasaEspecifica_t(eventos, poblacion)
#' tasas
#'
#' @export
DemBas_dem_TasaEspecifica_t <- function(vNumEventos_x_t, vPobMedia_x_t) {
  return((vNumEventos_x_t / vPobMedia_x_t) * 1000)
}


#' Calcula el Indicador Sintético de Fecundidad (ISF) coyuntural
#'
#' Calcula el ISF también conocido como Tasa de Fecundidad Total o
#' número medio de hijos por mujer. Es la suma de las tasas específicas
#' de fecundidad por edad (mide la fecundidad hipotética de una cohorte
#' si experimentara las tasas observadas en el período).
#'
#' @param vTasasEspecificas_x_t Vector numérico con las tasas específicas
#'   de fecundidad por grupo de edad. Debe estar expresado en tasas por 1
#'   (no por 1000).
#'
#' @return Valor numérico con el número medio de hijos por mujer.
#'
#' @examples
#' # ISF a partir de tasas específicas de fecundidad
#' tasas_fecundidad <- c(0.001, 0.005, 0.015, 0.025, 0.035, 0.040,
#'                       0.030, 0.015, 0.005, 0.001)
#' isf <- DemBas_dem_ISinteticoCoyuntural_t(tasas_fecundidad)
#' isf
#'
#' @export
DemBas_dem_ISinteticoCoyuntural_t <- function(vTasasEspecificas_x_t) {
  return(sum(vTasasEspecificas_x_t, na.rm = TRUE) / 1000)
}


#' Calcula la edad media de un evento demográfico
#'
#' Calcula la edad media al ocurrencia de un evento demográfico
#' (ej. edad media de la madre al nacimiento de sus hijos, edad media
#' al matrimonio, etc.). Se calcula como la media ponderada de las
#' marcas de clase de edad por las tasas específicas.
#'
#' @param vTasasEspecificas_x_t Vector numérico con las tasas específicas
#'   del evento por grupo de edad.
#' @param Edad.marcas Vector numérico con las marcas de clase de cada
#'   grupo de edad (ej. 2.5 para el grupo 0-4, 7 para el 5-9, etc.).
#'   Debe tener la misma longitud que vTasasEspecificas_x_t.
#'
#' @return Valor numérico con la edad media del evento.
#'
#' @examples
#' # Edad media de la madre al nacimiento
#' tasas <- c(0.001, 0.005, 0.015, 0.025, 0.035, 0.040, 0.030, 0.015, 0.005)
#' marcas <- c(2.5, 7, 12, 17, 22, 27, 32, 37, 42)
#' edad_media <- DemBas_dem_EdadMedia_t(tasas, marcas)
#' edad_media
#'
#' @export
DemBas_dem_EdadMedia_t <- function(vTasasEspecificas_x_t, Edad.marcas) {
  numer <- sum(vTasasEspecificas_x_t * Edad.marcas, na.rm = TRUE) / 1000
  denom <- DemBas_dem_ISinteticoCoyuntural_t(vTasasEspecificas_x_t)
  return(numer / denom)
}



###
#### OTRAS UTILIDADES
###


#' Elimina el código de comunidad autónoma de una columna de texto
#'
#' Elimina los dos dígitos iniciales de código de comunidad autónoma
#' (ej. "01", "02", ..., "19") de los nombres de territorios que
#' aparecen en los datos del INE. Este código va seguido de un espacio
#' y el nombre completo de la comunidad.
#'
#' @param columna Vector de tipo character con los nombres que incluyen
#'   el código de comunidad autónoma al inicio (ej. "01 Andalucía",
#'   "02 Aragón").
#'
#' @return Vector de tipo character con los nombres sin el código
#'   inicial y sin espacios adicionales (ej. "Andalucía", "Aragón").
#'
#' @examples
#' nombres_con_codigo <- c("01 Andalucía", "02 Aragón", "03 Asturias")
#' nombres_limpios <- DemBas_elimina_codigo_CCAA(nombres_con_codigo)
#' nombres_limpios
#'
#' @export
DemBas_elimina_codigo_CCAA <- function(columna) {

  pattern <- "\\d\\d\\s"
  reemplazo <- ""
  nombre <- as.character(stringr::str_replace_all(columna, pattern, reemplazo))
  nombre <- stringr::str_trim(nombre)
  return(nombre)
}


#' Extrae el código numérico de comunidad autónoma de una columna de texto
#'
#' Obtiene los dos dígitos iniciales de código de comunidad autónoma
#' de los nombres de territorios que aparecen en los datos del INE.
#'
#' @param columna Vector de tipo character con los nombres que incluyen
#'   el código de comunidad autónoma al inicio (ej. "01 Andalucía",
#'   "02 Aragón").
#'
#' @return Vector de tipo character con los códigos de comunidad
#'   autónoma de dos dígitos (ej. "01", "02", "03").
#'
#' @examples
#' nombres_con_codigo <- c("01 Andalucía", "02 Aragón", "03 Asturias")
#' codigos <- DemBas_extrae_codigo_CCAA(nombres_con_codigo)
#' codigos
#'
#' @export
DemBas_extrae_codigo_CCAA <- function(columna) {
  pattern <- "\\d\\d"
  codigo <- as.character(stringr::str_extract(columna, pattern))
  codigo <- stringr::str_trim(codigo)
  return(codigo)
}


#' Identifica los valores que no tienen código asignado
#'
#' Encuentra los elementos de una columna que no tienen un código
#' correspondiente en la columna de códigos (valores NA en la columna
#' de códigos).
#'
#' @param columna Vector de tipo character con los nombres originales.
#' @param columnacodigos Vector de tipo character o numeric con los
#'   códigos correspondientes. Los valores NA indican elementos sin código.
#'
#' @return Vector de tipo character con los valores únicos de columna
#'   que no tienen código asignado (es decir, cuyo código es NA).
#'
#' @examples
#' nombres <- c("Andorra", "España", "Francia", "Alemania")
#' codigos <- c(NA, "724", "250", NA)
#' sin_codigo <- DemBas_extrae_notienen_codigos(nombres, codigos)
#' sin_codigo
#'
#' @export
DemBas_extrae_notienen_codigos <- function(columna, columnacodigos) {
  ind <- which(is.na(columnacodigos))
  salida <- unique(columna[ind])
  return(salida)
}


#' Asigna un código personalizado a un valor específico de una columna
#'
#' Reemplaza el código de un elemento específico en la columna de
#' códigos por un nuevo código proporcionado. Útil para estandarizar
#' códigos o corregir inconsistencias en los datos.
#'
#' @param columna Vector de tipo character con los nombres originales.
#' @param columna_codigos Vector de tipo character o numeric con los
#'   códigos correspondientes.
#' @param etiqueta Valor de tipo character que identifica el elemento
#'   de columna cuyo código se desea modificar.
#' @param nuevocodigo Valor que se asignará como código al elemento
#'   identificado por etiqueta.
#'
#' @return Vector con los códigos originales, donde el elemento
#'   correspondiente a etiqueta ha sido reemplazado por nuevocodigo.
#'
#' @examples
#' nombres <- c("España", "Francia", "Alemania")
#' codigos <- c("724", "250", "276")
#' codigos_mod <- DemBas_asigna_codigos_CCAA(nombres, codigos, "España", "ES")
#' codigos_mod
#'
#' @export
DemBas_asigna_codigos_CCAA <- function(columna, columna_codigos,
                                        etiqueta, nuevocodigo) {
  ind <- which(columna == etiqueta)
  columna_codigos[ind] <- nuevocodigo
  return(columna_codigos)
}


#' Extrae la edad numérica de una columna de texto con formato
#'
#' Extrae el número inicial de edad de cadenas de texto que contienen
#' la edad seguida de texto descriptivo (ej. "25 años", "3 años",
#' "0 meses"). Útil para limpiar datos de edad provenientes de fuentes
#' estadísticas.
#'
#' @param columna Vector de tipo character con las cadenas de texto
#'   que contienen la edad al inicio (ej. "25 años", "0 edad").
#'
#' @return Vector de tipo character con los números de edad extraídos
#'   (ej. "25", "0").
#'
#' @examples
#' textos_edad <- c("25 años", "3 años", "0 edad", "75 años")
#' edades <- DemBas_extrae_Num_Edad(textos_edad)
#' edades
#'
#' @export
DemBas_extrae_Num_Edad <- function(columna) {
  pattern <- "\\d+\\s"
  codigo <- as.character(stringr::str_extract(columna, pattern))
  codigo <- stringr::str_trim(codigo)
  return(codigo)
}


# Estaban en funciones_piramides.R ------


#' Agrupa una variable continua en grupos de edad quinquenales
#'
#' Agrupa valores numéricos de edad en grupos quinquenales (0-4, 5-9,
#' 10-14, ...) utilizando diferentes métodos de agrupación. Los grupos
#' resultantes son factores que pueden usarse directamente en tablas
#' y gráficos de pirámides poblacionales.
#'
#' @param variable Vector numérico con los valores de edad a agrupar.
#'   Debe ser de tipo numeric o integer.
#' @param metodo Valor entero que indica el método de agrupación:
#'   \describe{
#'     \item{1}{Grupos: 0, 1-4, 5-9, 10-14, ... (edad 0 separada)}
#'     \item{2}{Grupos: 0-4, 5-9, 10-14, 15-19, ... (todos quinquenales)}
#'   }
#'   Por defecto 1.
#' @param final Valor numérico con la edad final para el último grupo
#'   antes del grupo abierto (ej. 85 significa que el último grupo cerrado
#'   es 85-89 si metodo=1, o 80-84 si metodo=2). Por defecto 85.
#' @param vbreaks Vector numérico opcional con los puntos de corte
#'   personalizados para los grupos. Si se especifica, se ignoran
#'   los parámetros metodo y final. Por defecto NULL.
#' @param vlabels Vector de caracteres opcional con las etiquetas
#'   personalizadas para cada grupo definido por vbreaks. Por defecto NULL.
#' @param labelfinal Cadena de texto opcional con la etiqueta para el
#'   grupo final abierto (ej. "85+", "100+"). Si es NULL, se usa
#'   paste0(final, "+"). Por defecto NULL.
#'
#' @return Factor con los grupos de edad resultantes de la agrupación.
#'   El nivel final incluye todas las edades mayores o iguales al
#'   límite especificado.
#'
#' @examples
#' # Agrupar edades con método 1 (0 separado)
#' edades <- c(0, 2, 5, 23, 45, 67, 88, 95, 105)
#' grupos1 <- DemBas_agrupar_variable(edades, metodo = 1, final = 85)
#' grupos1
#'
#' # Agrupar edades con método 2 (todos quinquenales)
#' grupos2 <- DemBas_agrupar_variable(edades, metodo = 2, final = 85)
#' grupos2
#'
#' # Usar puntos de corte personalizados
#' grupos_custom <- DemBas_agrupar_variable(edades,
#'                                          vbreaks = c(0, 18, 35, 65, 150),
#'                                          vlabels = c("Niños", "Jóvenes",
#'                                                      "Adultos", "Mayores"))
#' grupos_custom
#'
#' @export
DemBas_agrupar_variable <- function(variable, metodo = 1, final = 85,
                                     vbreaks = NULL, vlabels = NULL,
                                     labelfinal = NULL) {

  if (is.null(labelfinal)) {
    lbfinal <- paste0(final, "+")
  } else {
    lbfinal <- labelfinal
  }

  if (is.null(vbreaks)) {
    if (metodo == 1) {
      Agrup <- cut(variable, breaks = c(0, 1, seq(5, final, by = 5), 200),
                   right = FALSE,
                   labels = c("0", "1-4",
                              paste0(seq(5, final - 5, by = 5), "-",
                                     seq(9, final, by = 5)),
                              lbfinal))
      return(Agrup)
    }
    if (metodo == 2) {
      Agrup <- cut(variable, breaks = c(seq(0, final, by = 5), 200),
                   right = FALSE,
                   labels = c(paste0(seq(0, final - 5, by = 5), "-",
                                     seq(4, final, by = 5)),
                              lbfinal))
      return(Agrup)
    }
  } else {
    if (is.null(vlabels)) {
      Agrup <- cut(variable, breaks = vbreaks, right = FALSE)
    } else {
      Agrup <- cut(variable, breaks = vbreaks, right = FALSE,
                   labels = vlabels)
    }
    return(Agrup)
  }


}


####
####
####


#' Genera etiquetas para grupos de edad quinquenales
#'
#' Crea un vector de etiquetas de texto para grupos de edad
#' quinquenales, útil para etiquetar resultados de agrupaciones
#' o crear tablas de referencia. Admite dos métodos de etiquetado.
#'
#' @param metodo Valor entero que indica el método de generación:
#'   \describe{
#'     \item{1}{Etiquetas: "0", "1-4", "5-9", "10-14", ... (edad 0 separada)}
#'     \item{2}{Etiquetas: "0-4", "5-9", "10-14", "15-19", ... (todos quinquenales)}
#'   }
#'   Por defecto 1.
#' @param final Valor numérico con la edad final para la última
#'   etiqueta antes del grupo abierto. Por defecto 85.
#' @param labelfinal Cadena de texto opcional con la etiqueta para el
#'   grupo final abierto. Si es NULL, se usa paste0(final, "+").
#'   Por defecto NULL.
#'
#' @return Vector de caracteres con las etiquetas de los grupos de
#'   edad (ej. c("0", "1-4", "5-9", ..., "85+")).
#'
#' @examples
#' # Método 1: 0 separado
#' etiquetas1 <- DemBas_etiquetas_gruposEdad(metodo = 1, final = 85)
#' head(etiquetas1, 10)
#'
#' # Método 2: todos quinquenales
#' etiquetas2 <- DemBas_etiquetas_gruposEdad(metodo = 2, final = 85)
#' head(etiquetas2, 10)
#'
#' # Grupo final personalizado
#' etiquetas_custom <- DemBas_etiquetas_gruposEdad(final = 100,
#'                                                  labelfinal = "100+")
#' tail(etiquetas_custom)
#'
#' @export
DemBas_etiquetas_gruposEdad <- function(metodo = 1, final = 85,
                                         labelfinal = NULL) {

  if (is.null(labelfinal)) {
    lbfinal <- paste0(final, "+")
  } else {
    lbfinal <- labelfinal
  }

  if (metodo == 1) {
    Agrup <- c("0", "1-4", paste0(seq(5, final - 5, by = 5), "-",
                                  seq(9, final, by = 5)), lbfinal)
    return(Agrup)
  }
  if (metodo == 2) {
    Agrup <- c(paste0(seq(0, final - 5, by = 5), "-",
                      seq(4, final, by = 5)), lbfinal)
    return(Agrup)
  }

}




# Estaban en funciones_mapas.R ------


#' Extrae el código numérico de provincia del nombre
#'
#' Obtiene los dígitos iniciales de código de provincia de los
#' nombres de provincia que aparecen en los datos del INE. El código
#' va separado del nombre por un espacio en blanco.
#'
#' @param vprovincias Vector de tipo character con los nombres de
#'   provincia que incluyen el código al inicio (ej. "01 Albacete",
#'   "02 Alicante").
#'
#' @return Vector de tipo character con los códigos de provincia
#'   extraídos (ej. "01", "02").
#'
#' @examples
#' nombres_provincias <- c("01 Albacete", "02 Alicante", "03 Almería")
#' codigos <- DemBas_extrae_codigo_provincia(nombres_provincias)
#' codigos
#'
#' @export
DemBas_extrae_codigo_provincia <- function(vprovincias) {

  resul <- sapply(vprovincias,
                  function(x) strsplit(x = as.character(x), split = " ")[[1]][1])
  return(resul)
}


#' Genera una paleta de colores usando RColorBrewer
#'
#' Crea un vector de colores para visualization de datos usando
#' las paletas disponibles en el paquete RColorBrewer. Permite
#' seleccionar la paleta por nombre o por número de índice.
#'
#' @param cuantos Valor entero con el número de colores a generar.
#'   Debe estar dentro del rango válido para la paleta seleccionada
#'   (generalmente entre 3 y 11).
#' @param que_paletacolor Puede ser:
#'   \describe{
#'     \item{Valor entero}{Índice de la paleta (1 a 18) según la lista
#'       de paletas disponibles.}
#'     \item{Cadena de texto}{Nombre de una paleta válida de
#'       RColorBrewer (ej. "Blues", "YlOrRd", "PuBu").}
#'   }
#'   Por defecto 3 (paleta "BuPu").
#'
#' @return Vector de caracteres con los códigos hexadecimales de
#'   los colores generados.
#'
#' @examples
#' # Generar 5 colores de la paleta por defecto (BuPu)
#' colores1 <- DemBas_crea_colores_brewer(5)
#' colores1
#'
#' # Generar 7 colores de la paleta "YlOrRd"
#' colores2 <- DemBas_crea_colores_brewer(7, que_paletacolor = "YlOrRd")
#' colores2
#'
#' # Usar índice de paleta (3 = BuPu)
#' colores3 <- DemBas_crea_colores_brewer(9, que_paletacolor = 3)
#' colores3
#'
#' @export
DemBas_crea_colores_brewer <- function(cuantos, que_paletacolor = 3) {
  paletas <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges",
               "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds",
               "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")
  if (is.character(que_paletacolor)) {
    cual <- which(que_paletacolor %in% paletas)
    if (cual > 0) {
      paletacolor <- que_paletacolor
    } else {
      paletacolor <- "BuPu"
    }
  } else {
    paletacolor <- paletas[que_paletacolor]
  }
  colors <- RColorBrewer::brewer.pal(cuantos, paletacolor)
  return(colors)
}


#' Extrae el código numérico de comunidad autónoma del nombre
#'
#' Obtiene los dígitos iniciales de código de comunidad autónoma
#' de los nombres que aparecen en los datos. Opcionalmente puede
#' convertir los códigos al formato SIANE (sumando 60 al código
#' numérico).
#'
#' @param vCCAA Vector de tipo character con los nombres que incluyen
#'   el código de comunidad autónoma al inicio (ej. "01 Andalucía",
#'   "02 Aragón").
#' @param ConvierteCodSIANE Valor lógico. Si TRUE, convierte los
#'   códigos al formato SIANE sumando 60 (ej. "01" -> "61").
#'   Si FALSE, devuelve los códigos originales. Por defecto TRUE.
#'
#' @return Vector de caracteres o numéricos con los códigos de
#'   comunidad autónoma extraídos y convertidos según el parámetro
#'   ConvierteCodSIANE.
#'
#' @examples
#' nombres_ccaa <- c("01 Andalucía", "02 Aragón", "03 Asturias")
#'
#' # Formato original
#' codigos_orig <- DemBas_extrae_codigo_ccaa(nombres_ccaa,
#'                                           ConvierteCodSIANE = FALSE)
#' codigos_orig
#'
#' # Formato SIANE
#' codigos_siane <- DemBas_extrae_codigo_ccaa(nombres_ccaa,
#'                                            ConvierteCodSIANE = TRUE)
#' codigos_siane
#'
#' @export
DemBas_extrae_codigo_ccaa <- function(vCCAA, ConvierteCodSIANE = TRUE) {

  resul <- sapply(vCCAA,
                  function(x) strsplit(x = as.character(x), split = " ")[[1]][1])
  if (ConvierteCodSIANE) {
    resul <- as.numeric(resul) + 60
  }
  return(resul)
}



#' Añade columnas de grupos de edad quinquenales a un data.frame
#'
#' Añade dos nuevas columnas a un data.frame: una con etiquetas
#' de grupos de edad quinquenales (ej. " 0-4", " 5-9", "10-14", ...)
#' y otra con el valor numérico inicial de cada grupo (ej. 0, 5, 10, ...).
#' Esta función es útil para preparar datos para la construcción de
#' pirámides poblacionales.
#'
#' @param datos data.frame o tibble que contiene una columna de edad
#'   numérica.
#' @param varEdad El nombre de la columna de edad (de tipo numeric o
#'   integer) presente en datos. Se especifica sin comillas (operador
#'   de evaluación no estándar de tidyselect).
#'
#' @return El mismo data.frame de entrada con dos columnas adicionales:
#'   \describe{
#'     \item{GEdad5}{Vector de caracteres con las etiquetas de los
#'       grupos quinquenales (ej. " 0-4", " 5-9", ..., "100+")}
#'     \item{GEdad5Num}{Vector numérico con la edad inicial de cada
#'       grupo (ej. 0, 5, 10, ..., 100)}
#'   }
#'
#' @examples
#' \dontrun{
#' # Requiere datos del paquete
#' datos <- DemBas_read_px(system.file("examples/9663.px",
#'                                     package = "DemographyBasic"))
#' datosPob <- datos |>
#'   dplyr::filter(Periodo == "1 de enero de  2018",
#'                 Edad.simple != "Total",
#'                 Sexo != "Ambos sexos") |>
#'   dplyr::select(Sexo, Edad.simple, Poblacion = value)
#'
#' datosPob2 <- datosPob |>
#'   dplyr::mutate(
#'     Edad = as.numeric(gsub("[años|año]", "", Edad.simple)),
#'     Poblacion = round(Poblacion, 0)
#'   )
#'
#' datosPob2_conGruposEdad <- DemBas_anade_GEdad5(datosPob2, Edad)
#' head(datosPob2_conGruposEdad, 15)
#'
#' # Agrupar para pirámide
#' datosPirAgru <- DemBas_anade_GEdad5(datosPob2, Edad) |>
#'   dplyr::select(Sexo, Edad, Poblacion, GEdad5) |>
#'   dplyr::group_by(GEdad5, Sexo) |>
#'   dplyr::summarise(Poblacion = sum(Poblacion), .groups = "keep")
#'
#' head(datosPirAgru)
#' }
#'
#' @export
DemBas_anade_GEdad5 <- function(datos, varEdad) {
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


