\name{detail}
\alias{detail}
\title{extend}
\usage{
  detail(data, namesou, ind.parent, ind.peers)
}
\arguments{
  \item{data}{a dataframe object.}

  \item{namesou}{a character vector specifying the main
  source and secondary source(s).}
}
\description{
  Extend time series
}
\details{
  Extend time series using growth rates of secondary
  sources
}
\examples{
data table: cou, var, ind, year, value
extend(data=data.ext.start, namesou=namesou.ext)
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{detail}}
}
\keyword{estimate}

