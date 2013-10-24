\name{packageData}
\alias{packageData}
\title{Package Data}
\usage{
  packageData(list = c("STAN", "BTD"), isic = 4,
    file = "data.rda", namecou = STAN.COU,
    namevar = STAN.VARALL, nameind = character(),
    nameyear = character(), replace = FALSE, ...)
}
\arguments{
  \item{list}{a character vector of National Accounts data
  source IDs.}

  \item{isic}{an integer specifying the ISIC classification
  of data sources.}

  \item{namecou}{a character vector of 3-digit ISO country
  codes read from column \sQuote{cou}.}

  \item{namevar}{a character vector of variables read from
  column \sQuote{var}.}

  \item{nameind}{a character vector of industries read from
  column \sQuote{ind} or from \sQuote{dim.ind} if
  specified.}

  \item{nameyear}{and integer vector specifying the
  selected period.}

  \item{replace}{specify if the existing data shall be
  replaced.}
}
\description{
  Package National Accounts data sources
}
\details{
  This function prepares R data files for packaging. It is
  primarily intended to maintain data sources from National
  Accounts (in million USD).
}
\examples{
packageData(list = c("WIOT042012"), isic = 3)
## Package multiple sources
packageData(list = c("STAN", "WIOT042012"), isic = 3, file = paste0(PATH.REPO, "stanData\\\\data\\\\STANNAi3.rda"))
packageData(list = c("STAN", "BTD"), isic = 4, file = paste0(PATH.REPO, "stanData\\\\data\\\\STANNAi4.rda"))
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{queryData}}, \code{\link{addDatalist}}
}
\keyword{package}

