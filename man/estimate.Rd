\name{estimate}
\alias{estimate}
\title{estimate}
\usage{
  estimate(data = data.all,
    sources = est.array.d[est.array.d$est == "DET", ],
    period = c(1995:2010))
}
\arguments{
  \item{data}{a dataframe object}

  \item{sources}{a data frame with main source and
  secondary sources for each combination in the data}

  \item{period}{time period for estimation}
}
\description{
  Create STAN estimates
}
\details{
  Combine extend and detail functions to time series
  according to supplied list of sources
}
\examples{
data table: cou, var, ind, year, value
sources table: cou, var, ind, est, MAIN, SEC
estimate(data=data.all, sources=est.array.d[est.array.d$est=="DET",], period=c(1995:2010))
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{extend}}, \code{\link{detail}}
}
\keyword{estimate}

