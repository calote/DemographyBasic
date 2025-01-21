library(pxR,quietly = TRUE)
# if (!requireNamespace("somepackage", quietly = TRUE)) {
#   stop("Please install 'somepackage' from GitHub: remotes::install_github('username/repo')")
# }
# Loading required package: stringr
# Loading required package: reshape2
# Loading required package: RJSONIO
# Loading required package: plyr

# Comentarios:
# Evita el error que se produce en MAC cuando importar ficheros px
# > datos = read.px("9681.px")
# > Error: C stack usage  11988528 is too close to the limit
# Nota:
# Se ha modificado únicamente la línea siguiente:
# FINAL:
# tmp <- str_split( a, "DATA=" )[[1]]
# INICIAL:
# tmp <- strsplit( a, "DATA=" )[[1]]
#
#
#library(stringr)
#library(plyr)

#datos = as.data.frame(read.px("9681.px"))
# datos = read.px("9681.px")
#
#
# datos = read.px("9681.px")
#
# datos2 = read.px("04003.px")
#
# rprofile = Sys.getenv('R_PROFILE_USER', '~/.Rprofile')
# if (file.exists(rprofile)) {
#   base::sys.source(rprofile, envir = environment())
# }
# rm(rprofile)





# redefinir read.px como read_px-----

read_px <- function(filename, encoding = NULL,
                    na.strings = c('"."', '".."', '"..."', '"...."', '":"')) {

  ## auxiliary functions ##
  #browser()
  clean.spaces <- function(x){
    gsub("^[[:space:]]+|[[:space:]]+$", "", x) # discards heading|trailing whitespace
  }

  get.attributes <- function(x){
    x <- gsub( "([A-Z-]*)\\((.*)\\).*", "\\1;\\2", x ) ## separates label-attribute with ";"
    x <- plyr::ldply(strsplit(x, ";"),
                     function(y) c(y, "value")[1:2])
  }

  break.clean <- function(x) {
    x <- clean.spaces( strsplit(x, split = '\\"')[[1]] )    ## breaks by '"'
    x[! x %in% c("," , "")]                                 ## and drops spurious seps
  }


  ## end: auxiliary functions ##

  # modification by  fvf (150211): Determining the character encoding used in the file => encoding

  if (is.null(encoding)) {
    charset  <- readLines(filename, 5)   # read the first five lines
    encoding <- ifelse(any(grepl('CHARSET.*ANSI', charset, ignore.case = T)),
                       "latin1", "CP437")  # comprobado en debian y osx
  }

  a <- scan(filename, what = "character", sep = "\n", quiet = TRUE, fileEncoding = encoding)

  # modification by  fvf: 130608
  a <- paste(a, collapse = "\n")        # Se mantienen "CR/LF luego se quitaran selectivamente

  tmp <- str_split( a, "DATA=" )[[1]]
  tmp[1] <- gsub("\n", " ", tmp[1])     # fvf[130608]: elimina CR de la cabecera
  tmp[2] <- gsub(";", "", tmp[2])       # fvf[150212] (la modificacion rev 92 a 94) da multiples problemas en INEBase
  # i.e: read.px('http://www.ine.es/pcaxisdl//t20/e245/p05/a2002/l0/00004001.px')
  # en muchos ficheros cada linea del area DATA tiene ";" antes del "EOL"
  # lo que produce que solo se lea la primera de las lineas de datos
  a <- paste(tmp[1], "DATA=", tmp[2], sep = "")

  ## modification by cjgb, 20130228 concerning line separators within quoted strings
  ## ; is the logical line end in px files
  ## so we should do:
  ## a <- unlist(strsplit(a, ";"))
  ## but there might be ; inside quoted strings
  ## so we need the following workaround:

  punto.coma <- str_locate_all(a, ";")[[1]][,1] # where the ";" are
  comillas   <- str_locate_all(a, '"')[[1]][,1] # where the '"' are

  ## ";" not after an odd number of '"'
  ## these are the proper "cuts"
  cortes     <- Filter( function(x) sum(comillas < x) %% 2 == 0, punto.coma )

  a <- str_sub(a, c(1, cortes + 1), c(cortes - 1, str_length(a)))
  a <- a[!is.na(a)]
  a <- a[a != ""]

  ## end of modification by cjgb, 20130228 concerning line separators within quoted strings


  # change strsplit by str-split. In big px-files:
  #  "Error: C stack usage is too close to the limit"
  #a <- do.call(rbind, str_split(a, "=", n = 2))
  a <- do.call(rbind, str_split(a, "=", n = 2))

  ## fvf.20141222: not chage to factor: ++ stringsAsFactors=F)
  a <- data.frame(cbind(get.attributes(a[, 1]), a[, 2], stringsAsFactors=F))

  colnames(a) <- c("label", "attribute", "value")

  ## build a px object: list with px class attribute ##

  a$label     <- make.names(clean.spaces(a$label))
  a$attribute <- make.names(clean.spaces(gsub('\\"', "", a$attribute)))

  # need to avoid that quotes are removed in DATA part because of a bug:
  # a case was reported where the data part ended in ".." and the last quote was erased
  # and this affected the scan function below
  a.data                     <- as.character(a[a$label == "DATA", "value"])
  a.value                    <- gsub('^\\"|\\"$', "", a$value)   # removes " at beginning / end
  a.value[a$label == "DATA"] <- a.data
  names(a.value)             <- a$attribute

  px <- tapply(a.value, a$label, as.list)

  ## these metadata keys contain vectors (comma separated)
  ## we need to split them (and clean the mess: extra spaces, etc.)
  px$STUB$value    <- if(!is.null(px$STUB))    make.names(break.clean(px$STUB$value))
  px$HEADING$value <- if(!is.null(px$HEADING)) make.names(break.clean(px$HEADING$value))

  px$VALUES <- lapply(px$VALUES, break.clean)

  # fvf.20141222: if there are not CODES, do not create CODES
  if (!is.null(px$CODES))
    px$CODES <- lapply(px$CODES, break.clean)

  # fvf.20141222: Sustituye ["~~~~" "~~~~~"] por ["~~~~~"\n"~~~~"]  en
  # campos multilinea con retornos perdidos (simplifica la lectura humana)

  px <- lapply(px, function(e){
    if (!is.null(e$value))
      e$value <- gsub('"[[:space:]]+"', '"\n"', e$value)
    e
  })

  #### read the data part into a 'melted' dataframe ###

  ## there are two cases: files with/without KEYS keyword
  ## which need to be processed independently

  # fvf[130608]: add to to read files with keys in data area

  if ("KEYS" %in% a$label ){

    ## read the whole block
    tc <- textConnection(px$DATA$value); on.exit( close(tc) )
    raw <- read.table(tc, sep = ",", colClasses = "factor")

    ## extract and process the data part (the numbers)
    data.part <- as.character(raw[, ncol(raw)] )          # numbers (last column of the data.frame)
    data.part <- gsub('"-"', 0, data.part)                # 0's might be encoded as "-"
    data.part <- scan(text = data.part, na.strings = na.strings, quiet = T)

    ## extract and process the keys part (it needs to be staked a number of times,
    ##  as many as there are entries in the data vector in each row in the block)
    keys.part <- raw[, -ncol(raw), drop = FALSE]
    keys.part <- keys.part[ rep(1:nrow(keys.part), each = length(data.part) / nrow(keys.part) ), , drop = FALSE ]
    colnames(keys.part) <- names(px$KEYS)

    ## change CODES (if any) in keys part to VALUES (consistency issue)
    # for (col.name in colnames(keys.part)[unlist(px$KEYS) == "CODES"])
    #  keys.part[[col.name]] <- mapvalues(keys.part[[col.name]],
    #                                     from = px$CODES[[col.name]],
    #                                     to   = px$VALUES[[col.name]])
    # fvf.20141222:
    for (col.name in colnames(keys.part)){
      if (px$KEYS[[col.name]] == 'CODES')   {
        keys.part[[col.name]]  <- factor(keys.part[[col.name]], levels = px$CODES[[col.name]])
        levels(keys.part[[col.name]]) <- px$VALUES[[col.name]]  ## all levels a VALUES
      } else  keys.part[[col.name]]  <- factor(keys.part[[col.name]], levels = px$VALUES[[col.name]] )
    }


    ## extract and process the variables that are not keys
    no.keys.part <- px$VALUES[setdiff(names(px$VALUES), names(px$KEYS))]
    no.keys.part <- expand.grid(rev(no.keys.part))

    ## put everything together & cleanup
    px$DATA$value <- data.frame( keys.part,
                                 no.keys.part,
                                 value = data.part,
                                 row.names = NULL)
  }
  else
  {
    tmp <- gsub('"-"', 0, px$DATA$value)        # 0 can be encoded as "-"
    tmp <- gsub("\n", " ", tmp)                 # delete CR/LF of DATA area fvf[130608]

    tc  <- textConnection(tmp); on.exit( close(tc) )
    raw <- scan(tc, na.strings = na.strings, quote = NULL, quiet = TRUE)

    names.vals <- c( rev(px$HEADING$value), rev( px$STUB$value ) )
    output.grid <- data.frame(do.call(expand.grid, px$VALUES[names.vals]))

    # sanity check: avoids the problem of "reclycling" of values if
    # the ratio of lenghts of variables and values is an exact integer
    if (nrow(output.grid) != length(raw))
      stop( "The input file is malformed: data and varnames length differ" )

    px$DATA$value           <- data.frame(output.grid, raw)
    colnames(px$DATA$value) <- c(names.vals, "value")

  }

  class(px) <- "px"
  px
}


#' @title DemBas_read_px
#' @description read a file px (INE, Eurostat, etc) and return a data.frame
#'
#' @param filename
#' @param encoding
#' @param na.strings
#'
#' @returns data.frame
#' @examples
#' # example code
#' # Importing a .px file
#' example_data_px <- DemBas_read_px(system.file("examples/56940.px", package = "DemographyBasic"))
#' head(example_data_px)
#' @export
DemBas_read_px <- function(filename, encoding = NULL,
                     na.strings = c('"."', '".."', '"..."', '"...."', '":"')) {
  dt1 = as.data.frame(read_px(filename, encoding = encoding, na.strings = na.strings), stringsAsFactors = FALSE)
  #  dt2 = as.data.frame(lapply(dt1,function(x) if (is.factor(x)) as.character(x) else x ), stringsAsFactors = FALSE)

  #my.px.data2   <-  as.data.frame( my.px.object,stringsAsFactors=FALSE )
  for (i in 1:ncol(dt1)) {
    if (is.factor(dt1[[i]])) {
      dt1[[i]] = as.character(dt1[[i]])
    }
  }



  return(dt1)
}




#' @title DemBas_read_px_encodingIECA
#' @description read a file px (IECA) and return a data.frame
#'
#' @param ficheropx character with the name of the file px
#'
#' @return data.frame with the data
#' @examples
#' d103965windows4 = DemBas_read_px_encodingIECA("ieca_export_103965.px")
#' @export
DemBas_read_px_encodingIECA = function(ficheropx) {
  datos <- readLines(ficheropx)
  datos_convertidos <- iconv(datos, from = "UTF-8", to = "Windows-1252")
  fichero_tmp = tempfile()
  writeLines(datos_convertidos, fichero_tmp)
  datos_bien = DemBas_read_px(fichero_tmp)
  return(datos_bien)

}




#' @title DemBas_url_px_IECA
#' @description Descarga un fichero px de la web del IECA a partir de su identificador
#'
#' @param id character with the id of the px file in the IECA web
#' @param ficheropx character with the name of the file px to save
#'
#' @return character with the url of the file px
#' @examples
#' url1 = DemBas_url_px_IECA("103965", "ieca_export_103965_ff.px")
#' df103965_ff = DemBas_read_px_encodingIECA("ieca_export_103965_ff.px")
#' @export
DemBas_url_px_IECA = function(id, ficheropx) {
  url = paste0("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=ac06fd14-e977-4799-ab4f-aa2c53ccf15c&type=2&foto=si&ejecutaDesde=&codConsulta=",id,"&consTipoVisua=JP")

  fichero = download.file(url,
                          destfile = ficheropx,
                          quiet = TRUE,
                          mode = "wb")
  return(url)
}


#' @title DemBas_import_px_IECA
#' @description Importa un fichero px de la web del IECA a partir de su identificador
#'
#' @param id character with the id of the px file in the IECA web
#' @param ficheropx character with the name of the file px to save.
#' If is NULL, a temporal file is created
#' @return data.frame with the data
#' @examples
#' df103965 = DemBas_import_px_IECA("103965")
#'
#' df103965 = DemBas_import_px_IECA("103965", "ieca_export_103965_ff4.px")
#'
#' @export
DemBas_import_px_IECA = function(id, ficheropx = NULL) {
  if (is.null(ficheropx)) {
    ficheropx2 = tempfile()
  } else {
    ficheropx2 = ficheropx
  }
  url = DemBas_url_px_IECA(id, ficheropx2)
  datos = DemBas_read_px_encodingIECA(ficheropx2)
  return(datos)
}


#' @title DemBas_url_px_INE
#' @description Descarga un fichero px de la web del INE a partir de su identificador
#' @param id character with the id of the px file in the INE web
#' @param ficheropx character with the name of the file px to save
#' @return character with the url of the file px
#' @examples
#' url1 = DemBas_url_px_INE("2855", "2855_ine.px")
#' @export
DemBas_url_px_INE = function(id, ficheropx) {
  url = paste0("https://www.ine.es/jaxiT3/files/t/es/px/",id,".px?nocab=1")
  download.file(url,
                destfile = ficheropx,
                quiet = TRUE,
                mode = "wb")
  return(url)
}



#' @title DemBas_import_px_INE
#' @description Importa un fichero px de la web del INE a partir de su identificador
#' @param id character with the id of the px file in the INE web
#' @param ficheropx character with the name of the file px to save.
#' If is NULL, a temporal file is created
#' @return data.frame with the data
#' @examples
#' df2855_ine = DemBas_import_px_INE("2855")
#'
#' df2855_ine2 = DemBas_import_px_INE("2855", "2855_ine.px")
#' @export
DemBas_import_px_INE = function(id, ficheropx = NULL) {
  if (is.null(ficheropx)) {
    ficheropx2 = tempfile()
  } else {
    ficheropx2 = ficheropx
  }
  url = DemBas_url_px_INE(id, ficheropx2)
  datos = DemBas_read_px(ficheropx2)
  return(datos)
}



#' @title DemBas_leer_metadatos_px_INE
#' @description Leer metadatos de un fichero px del INE
#' @param ficheropx character with the name of the file px
#' @return data.frame with the metadata, column 1 is the name of the variable and column 2 is the content
#' @examples
#' df2855 = DemBas_import_px_INE("2855", "2855_ine.px")
#' df2855_meta = DemBas_leer_metadatos_px_INE("ieca_export_103965_ff4.px")
#' @export
DemBas_leer_metadatos_px_INE = function(ficheropx) {
  #ficheropx = "2855_ine.px"
  meta_px = readLines(ficheropx, n = 100)
  n_max = suppressWarnings(grep("VALUES(", meta_px, fixed = TRUE))
  if (isTRUE(n_max>0)) {
    meta_px = meta_px[1:(n_max-1)]
  }
  meta_px_bien <- iconv(meta_px, from = "Windows-1252", to = "UTF-8")
  meta_px_bien2 = strsplit(meta_px_bien, "=")
  v1 = sapply(meta_px_bien2, function(x) length(x) )
  col01 = sapply(meta_px_bien2, function(x) if (length(x)>1) x[1] )
  ucol01 = unlist(col01)
  col02 = sapply(meta_px_bien2, function(x) if (length(x)>1) x[2] else paste0("XYZ-",x[1]) )
  col02m = gsub('"', "", col02, fixed = TRUE)
  siestan = grep("XYZ-",col02m)
  col02m_notas = gsub('XYZ-',"", col02m[siestan], fixed = TRUE)
  col02m_notas_final = paste0(c(col02m[min(siestan)-1],col02m_notas), collapse = "")
  col02m = col02m[-siestan]
  col02m[min(siestan)-1] = col02m_notas_final
  col02m = gsub(';',"",col02m, fixed = TRUE)
  meta_df = data.frame("Claves" = ucol01,
                       "Valores" = col02m)

  return(meta_df)
}




#' @title DemBas_leer_metadatos_px_IECA
#' @description Leer metadatos de un fichero px del IECA
#' @param ficheropx character with the name of the file px
#' @return data.frame with the metadata, column 1 is the name of the variable and column 2 is the content
#' @examples
#' df103965 = DemBas_import_px_IECA("103965", "ieca_export_103965_ff4.px")
#' df103965_meta = DemBas_leer_metadatos_px_IECA("ieca_export_103965_ff4.px")
#' @export
DemBas_leer_metadatos_px_IECA = function(ficheropx) {
  meta_px = readLines(ficheropx, n = 100)
  #meta_px
  n_max = suppressWarnings(grep("DATA=", meta_px, fixed = TRUE))
  if (isTRUE(n_max>0)) {
    meta_px = meta_px[1:(n_max-1)]
  }
  meta_px_bien = meta_px
  meta_px_bien2 = strsplit(meta_px_bien, "=")
  v1 = sapply(meta_px_bien2, function(x) length(x) )
  col01 = sapply(meta_px_bien2, function(x) if (length(x)>1) x[1] )
  ucol01 = unlist(col01)
  col02 = sapply(meta_px_bien2, function(x) if (length(x)>1) x[2] else paste0("XYZ-",x[1]) )
  col02m = gsub('"', "", col02, fixed = TRUE)
  siestan = grep("XYZ-",col02m)
  col02m_notas = gsub('XYZ-',"", col02m[siestan], fixed = TRUE)
  col02m_notas_final = paste0(c(col02m[min(siestan)-1],col02m_notas), collapse = "")
  col02m = col02m[-siestan]
  col02m[min(siestan)-1] = col02m_notas_final
  col02m = gsub(';',"",col02m, fixed = TRUE)
  meta_df = data.frame("Claves" = ucol01,
                       "Valores" = col02m)

  return(meta_df)
}







