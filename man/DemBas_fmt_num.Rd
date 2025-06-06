% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/presentardf.R
\name{DemBas_fmt_num}
\alias{DemBas_fmt_num}
\title{DemBas_fmt_num}
\usage{
DemBas_fmt_num(x, ndigitos = 5, quitacerosderecha = TRUE)
}
\arguments{
\item{quitacerosderecha}{}
}
\value{
devuelve un character con el número formateado
}
\description{
Formatea un número para presentación
}
\examples{
DemBas_fmt_num(5654.0832)
DemBas_fmt_num(5654)
DemBas_fmt_num(565400982.3000,2,T)
DemBas_fmt_num(565400982.3000,2,F)
DemBas_fmt_num(565400982.23713830023)
DemBas_fmt_num(565400982.23713830023,2)
}
