\name{hierarchy}
\alias{hierarchy}
\title{Hierarchy}
\usage{
  hierarchy(file, agg.exclude, parent = FALSE,
    order = FALSE)
}
\arguments{
  \item{file}{the aggregation matrix in csv format.}

  \item{agg.exclude}{character vector with aggregates to be
  excluded from the hierarchy.}

  \item{parent}{logical to list all parents of industry
  instead of members.}

  \item{order}{logical to create ordered vector of
  aggregates and contained sectors}
}
\description{
  Create STAN industry hierarchy
}
\details{
  Function to create an industry hierarchy based on a
  aggregation matrix with industries in columns and
  aggregates in rows. Membership is indicated with a '1'.
}
\examples{
hierarchy(file = paste0(PATH.COUi3, 'Aggregation_general_2digit_ISIC3.csv'))
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{indAggregate}}, \code{\link{loadDim}}
}
\keyword{dimensions}

