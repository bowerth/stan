\name{couCreate}
\alias{couCreate}
\title{COU Create}
\usage{
  couCreate(cou = stop("'cou' must be specified"),
    isic = 3, period = "2013-14", remove = FALSE)
}
\arguments{
  \item{cou}{a 3-digit country ISO code character string.}

  \item{isic}{an integer to specify the version of STAN.}

  \item{period}{a character string with update period.}

  \item{remove}{logical remove country from STAN SAS tables
  (country, units).}
}
\description{
  Create Country
}
\details{
  Sets up the SAS folder structure for a new country in
  STAN.
}
\examples{
couCreate(cou="BGR", isic=3)
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{couList}}, \code{\link{SASbatch}}
}
\keyword{SAS}

