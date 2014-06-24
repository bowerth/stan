\name{SAScouList}
\alias{SAScouList}
\title{SAS COU List}
\usage{
  SAScouList(namecou = c("LVA"), isic = 4)
}
\arguments{
  \item{cou}{a 3-digit country ISO code character string.}

  \item{isic}{an integer to specify the version of STAN.}
}
\description{
  Updates SAS country list
}
\details{
  Adds or removes country from SAS Country List with
  national currency.
}
\examples{
SAScouList(namecou=c("LVA"))
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{SAScouCreate}}
}
\keyword{SAS}

