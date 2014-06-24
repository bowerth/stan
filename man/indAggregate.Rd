\name{indAggregate}
\alias{indAggregate}
\title{Aggregate ISIC}
\usage{
  indAggregate(data = stop("'data' must be specified"),
    isic = 3, cumulative = FALSE, naAsZero = FALSE,
    fill2D = FALSE)
}
\arguments{
  \item{data}{a dataframe having STAN industry labels (ISIC
  Rev. 3 or ISIC Rev. 4) as column names.}

  \item{isic}{an integer specifying the ISIC classification
  of data sources.}

  \item{cumulative}{logical to cumulate values if aggregate
  already existing in data.}

  \item{naAsZero}{logical convert NAs to zero before
  calculating aggregates.}

  \item{fill2D}{logical add column with NA or zero for each
  missing 2-digit industry}
}
\description{
  Aggregate industries ISIC Rev. 3 and ISIC Rev. 4
}
\details{
  Calculate STAN ISIC Rev. 3 aggregates based on the A60
  list and STAN ISIC Rev. 4 aggregates based on the A88
  list.
}
\examples{
## ISIC Rev. 3:
x <- data.frame("cou"="JPN",
                "year"=c(rep(1995, 4), rep(1996,4)),
                "ind"=c("C01", "C02", "C05", "C10"),
                "value"=c(10, 10, 10, 10))
data <- dcast(x,cou + year ~ ind, value.var='value')
data.agg <- indAggregate(data=data, isic=3)
## ISIC Rev. 4:
x <- data.frame("cou"="JPN",
                "year"=c(rep(1995, 4), rep(1996,4)),
                "ind"=c("D01", "D02", "D03", "D05"),
                "value"=c(10, 10, 10, 10))
data <- dcast(x,cou + year ~ ind, value.var='value')
data.agg <- indAggregate(data=data, isic=4)
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{loadDim}}
}
\keyword{dimensions}

