
# DemographyBasic

<!-- badges: start -->
<!-- badges: end -->

Calculate Basic Procedures Of Demography.


Functions Have Been Implemented To Calculate: Life Tables (Complete And Abbreviated), 
Overall Rates, Specific Rates, Lexis Diagrams, Population Pyramids, And Demographic Maps.

## Installation

You can install the development version of DemographyBasic like so:

``` r
# Instalar el paquete desde GitHub
install.packages("remotes")
remotes::install_github("calote/DemographyBasic", build_vignettes = TRUE)
```

When installing the package 'DemographyBasic', it will ask where to install the necessary packages. In that case, it is recommended to **install only packages from CRAN**, and when asked whether to install them from the source code, initially select 'no'. If there are issues with the installation, repeat the process but select 'yes' for the installation from the source code.

## Example

This is a basic example which shows you how to solve a common problem:

```r
library(DemographyBasic)
(mx0 = 1733/441881) # TMI = D^t_0/N^t 
# Defunciones de menores de un año durante 2003: 1733
# Nacimientos en España en 2003: 441881
mx = c(mx0,0.00027, 0.00013, 0.00016, 0.00043, 0.00057, 0.00059, 0.00081,
     0.00115, 0.00174, 0.00258, 0.00376, 0.00569, 0.00818, 0.01346,
     0.02206, 0.03844, 0.06981, 0.12872, 0.21674, 0.31705, 0.48258)
tv = DemBas_tablavida_abreviada(mx)
tv
```

## Recomendation

In Quarto documents (and R Markdown), when you want to avoid messages and warnings 
when loading the "DemographyBasic" package with the R command:
`library(DemographyBasic)`, it is recommended to use it by including it within 
the `suppressPackageStartupMessages()` function call as shown in the following code:

```r
suppressPackageStartupMessages(library(DemographyBasic))
(mx0 = 1733/441881) # TMI = D^t_0/N^t 
# Defunciones de menores de un año durante 2003: 1733
# Nacimientos en España en 2003: 441881
mx = c(mx0,0.00027, 0.00013, 0.00016, 0.00043, 0.00057, 0.00059, 0.00081,
     0.00115, 0.00174, 0.00258, 0.00376, 0.00569, 0.00818, 0.01346,
     0.02206, 0.03844, 0.06981, 0.12872, 0.21674, 0.31705, 0.48258)
tv = DemBas_tablavida_abreviada(mx)
tv
```

Please note that when loading the "DemographyBasic" package, the following libraries 
are automatically loaded in the order shown below, thus avoiding conflicts between 
commonly used functions with the "tidyverse" system:

```
pxR
stringr
reshape2
jsonlite
plyr
tidyverse
ggthemes
ggh4x
glue
mapSpain
kableExtra
```

Therefore, it is not recommended to load any of these libraries

- either before 
- or after 

having loaded the "DemographyBasic" package.
