
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
remotes::install_github("calote/DemographyBasic")
```

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

