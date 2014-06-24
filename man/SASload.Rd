\name{SASload}
\alias{SASload}
\title{SAS load}
\usage{
  SASload(cou = stop("'cou' must be specified"), isic = 4,
    sources = list(BGR = c("NSO", "SUT", "STD-SNA")),
    delete = FALSE, load.swn = TRUE, period = "2013-14",
    quiet = FALSE)
}
\arguments{
  \item{cou}{a 3-digit country ISO code character string.}

  \item{isic}{an integer to specify the version of STAN.}

  \item{sources}{a list naming the sources to load.}

  \item{delete}{a logical to delete data already existing
  in SWN.}

  \item{load.swn}{a logical to re-load data from SAS STAN
  libary into SWN.}

  \item{period}{a character string with the update period
  in file names.}

  \item{quiet}{a logical to return the created SAS program
  contents.}
}
\description{
  Create SAS loading programs
}
\details{
  Create or modify SAS loading programs for STAN based on a
  list of countries and sources.
}
\examples{
SASload(cou="BGR", sources=list(BGR = c('NSONA', 'SUT')))
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{SASbatch}}, \code{\link{SASestim}},
  \code{\link{SASpub}}
}
\keyword{SAS}

