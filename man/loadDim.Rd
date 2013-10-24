\name{loadDim}
\alias{loadDim}
\title{Load Dimensions}
\usage{
  loadDim(dim = c("cou", "var", "indi4", "indi3"),
    file = paste0(PATH.REPO, "stan\\data\\stanDim.rda"),
    datalist = TRUE, ...)
}
\arguments{
  \item{dim}{a list of dimension to be loaded.}

  \item{file}{a data file}

  \item{datalist}{a boolean expression if the file and its
  contents shall be added to a package data list}
}
\description{
  Load STAN dimension members
}
\details{
  Function to create a data file with the dimension members
  used in STAN.
}
\examples{
loadDim(dim = c("cou", "var"), file = "data.rda", datalist = TRUE)
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{indAggregate}}
}
\keyword{dimensions}

