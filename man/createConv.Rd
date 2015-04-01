\name{createConv}
\alias{createConv}
\title{Create Converter}
\usage{
  createConv(dim = list(row = c(58, 37), col = c(58)),
    aggdim = c(1, 2), aggrng = c(1, 1), fixed = 1,
    horiz = FALSE, dimnames = NULL)
}
\arguments{
  \item{dim}{a list with two non-empty elements specifying
  the dimensions of the converter.}

  \item{aggdim}{a numeric vector with two elements
  specifying the dimensions within to aggregate members.}

  \item{aggrng}{a numeric vector with two elements
  specifying the range of aggregation within the dimension
  selected by \code{aggdim}.}

  \item{horiz}{logical specifying if the converter shall be
  applied horizontally.}
}
\description{
  Create Matrix Aggregation Converters
}
\details{
  Create converters for the aggregation of matrices. Tested
  with two cascaded row dimensions and one column
  dimension.
}
\examples{
createConv(dim=list(row=c(10,5),col=c(10)),aggdim=c(1,2),aggrng=c(1,3),fixed=2,horiz=FALSE)
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{indAggregate}}
}
\keyword{IO}

