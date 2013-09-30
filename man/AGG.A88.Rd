\name{AGG.A88}
\alias{AGG.A88}
\title{Get estate}
\usage{
  AGG.A88(data = NULL)
}
\arguments{
  \item{type}{choose to scrape offers for rent or sale.}

  \item{pages}{select the number of pages, each of them
  containing 40 items.}
}
\description{
  Get real estate data
}
\details{
  This function scrapes data from \code{\link{pap.fr}}
  using the \code{XML} and the \code{stringr} package.
}
\examples{
data <- getEstate(type = "vente", pages = 50)
}
\author{
  Bo Werth
}
\seealso{
  \code{www.pap.fr}
}
\keyword{scrape}

