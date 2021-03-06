% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XLStransform.R
\name{XLStransform}
\alias{XLStransform}
\title{XLS transform (EU)}
\usage{
XLStransform(cou = stop("'cou' must be specified"), isic = 4,
  append = FALSE)
}
\arguments{
\item{cou}{a character string 3-digit ISO code specifying the country.}

\item{isic}{industry classification to be used for the data output locacation.}

\item{append}{a boolean expression if variables shall be added to those already present.}
}
\description{
XLS Questionnaire transformation (EU)
}
\details{
Transform EU Excel Questionnaire for loading in SAS.
}
\examples{
XLStransform(cou="ITA", isic=4, append=FALSE)
}
\author{
OECD STAN
}
\seealso{
\code{\link{ANA2XLS}}
}
\keyword{XLS}

