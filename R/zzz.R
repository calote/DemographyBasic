.onLoad <- function(libname, pkgname) {
  suppressPackageStartupMessages({
    requireNamespace("pxR", quietly = TRUE)
    requireNamespace("kableExtra", quietly = TRUE)
    requireNamespace("legendry", quietly = TRUE)
    requireNamespace("dplyr", quietly = TRUE)
    requireNamespace("ggplot2", quietly = TRUE)
    requireNamespace("tibble", quietly = TRUE)
    requireNamespace("stringr", quietly = TRUE)
    requireNamespace("forcats", quietly = TRUE)
  })
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("✓ DemographyBasic listo")
}
