.onLoad <- function(libname, pkgname) {
  suppressPackageStartupMessages({
    requireNamespace("pxR", quietly = TRUE)
    requireNamespace("tidyverse", quietly = TRUE)
    requireNamespace("ggthemes", quietly = TRUE)
    requireNamespace("ggh4x", quietly = TRUE)
    requireNamespace("glue", quietly = TRUE)
    requireNamespace("mapSpain", quietly = TRUE)
    requireNamespace("kableExtra", quietly = TRUE)
    requireNamespace("LexisPlotR", quietly = TRUE)
  })
}

.onAttach <- function(libname, pkgname) {
  # Mensaje opcional muy discreto
  packageStartupMessage("✓ DemographyBasic listo (pxR,tidyverse,ggthemes,ggh4x,glue,mapSpain,kableExtra,LexisPlotR también)")
}
