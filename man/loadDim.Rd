% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadDim.R
\name{loadDim}
\alias{loadDim}
\title{Load Dimensions}
\usage{
loadDim(dim = c("cou", "var", "indi4", "indi3", "hierarchyi3", "cur"),
  file = file.path(PATH.REPO, "stan", "data", "stanDim.rda"),
  datalist = TRUE, replace = FALSE, ...)
}
\arguments{
\item{dim}{a character vector of dimension to be loaded.}

\item{file}{Rdata file where objects will be stored.}

\item{datalist}{a boolean expression if the file and its contents shall be added to a package data list.}

\item{replace}{specify if the existing data shall be replaced.}
}
\description{
Load dimension members
}
\details{
Function to create a data file with the dimension members used in STAN.
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

