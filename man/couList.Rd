\name{couList}
\alias{couList}
\title{COU List}
\usage{
  couList(namecou = c("LVA"), isic = 4)
}
\arguments{
  \item{cou}{a 3-digit country ISO code character string.}

  \item{isic}{an integer to specify the version of STAN.}
}
\description{
  Updates country list
}
\details{
  Adds or removes country from SAS Country List with
  national currency.
}
\examples{
couList(namecou=c("LVA"))
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{couCreate}}
}
\keyword{SAS}

