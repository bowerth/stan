\name{NSO2XLS}
\alias{NSO2XLS}
\title{NSO2XLS}
\usage{
  NSO2XLS(data = NULL,
    cou = stop("'cou' must be specified"), sou = "NSO",
    cover = paste0(PATH.COUi4, cou, "/Rawdata/", cou, "_", sou, "_4SAS_template.xls"),
    file = paste0(PATH.COUi4, cou, "/Rawdata/R_", cou, "_", sou, "_4SAS_2013-14.xls"))
}
\arguments{
  \item{data}{a dataframe object.}

  \item{cou}{a character string 3-digit ISO code specifying
  the country.}

  \item{sou}{a character string specifying the source.}

  \item{cover}{path to the template Excel workbook
  containing information about data units.}

  \item{file}{path to the output data file. Default needs
  to be modified with each update cycle.}
}
\description{
  Convert NSO data to XLS
}
\details{
  Convert dataframe for one source to XLS SAS input format.
}
\examples{
NSO2XLS(cou="ITA")
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{ANA2XLS}}
}
\keyword{SAS}

