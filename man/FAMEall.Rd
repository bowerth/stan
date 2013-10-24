\name{FAMEall}
\alias{FAMEall}
\title{FAME all}
\usage{
  FAMEall(cou = stop("'cou' must be specified"), isic = 4,
    classification = "NACE2", year = 2012,
    tables = c(301, 302, 303, 2000), version = "latest",
    ...)
}
\arguments{
  \item{cou}{a 3-digit country ISO code character string.}

  \item{year}{an integer specifying the year of interest.}

  \item{isic}{a character sring matching the industry
  classification used in the dataset name.}

  \item{tables}{an integer vector specifying the SNA tables
  to be looked for.}

  \item{version}{optional: the version number of the file
  to be used.}
}
\description{
  Combine FAME functions
}
\details{
  Call FAMEprepare, FAMEload and FAMEtransform together.
}
\examples{
FAMEall(cou="ITA")
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{FAMEprepare}}, \code{\link{FAMEload}},
  \code{\link{FAMEtransform}}
}
\keyword{FAME}

