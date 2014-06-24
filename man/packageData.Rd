\name{packageData}
\alias{packageData}
\title{Package Data}
\usage{
  packageData(list = c("STAN", "BTD"), isic = 4,
    file = "data.rda", namecou = character(),
    namepar = character(), namevar = character(),
    nameeuc = character(), nameind = character(),
    nameyear = character(), replace = FALSE,
    sqlite = FALSE, ...)
}
\arguments{
  \item{list}{a character vector of National Accounts data
  source IDs.}

  \item{isic}{an integer specifying the ISIC classification
  of data sources.}

  \item{namecou}{a character vector of 3-digit ISO country
  codes read from column \sQuote{cou}.}

  \item{namepar}{a character vector of 3-digit ISO partner
  codes read from column \sQuote{par}.}

  \item{namevar}{a character vector of variables read from
  column \sQuote{var}.}

  \item{nameeuc}{a character vector of end-use codes read
  from column \sQuote{euc}.}

  \item{nameind}{a character vector of industries read from
  column \sQuote{ind} or from \sQuote{dim.ind} if
  specified.}

  \item{nameyear}{and integer vector specifying the
  selected period.}

  \item{replace}{specify if the existing data shall be
  replaced.}

  \item{sqlite}{specify if the data shall be stored as
  SQLite database.}
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
## create for stanapp:
sourcesSTANNAi3 <- c('UNSDSNA2013', 'ICIO052013', 'WIOT042012', 'STAN', 'BTD')
packageData(list=sourcesSTANNAi3,
            namecou = unique(c(union(STAN.COU, STAN.COUKPC), "BGR", "BRN", "CYP", "HKG", "KHM", "LKA", "LTU", "LVA", "MLT", "MYS", "ROU", "SAU", "SGP", "THA", "VNM")),
            namevar = union(STAN.VARALL, c("FDDE", "FGGE", "FHHE", "GCFI", "INVC", "GDPR")),
            isic = 3,
            file = file.path(PATH.REPO, "stanData\\\\data\\\\STANNAi3.rda"),
            replace = TRUE)

## include or update sources, keeping existing sources in file:
packageData(list = c("ICIO052013"),
            namecou = unique(c(union(STAN.COU, STAN.COUKPC), "BGR", "BRN", "CYP", "HKG", "KHM", "LKA", "LTU", "LVA", "MLT", "MYS", "ROU", "SAU", "SGP", "THA", "VNM")),
            namevar = union(STAN.VARALL, c("FDDE", "FGGE", "FHHE", "GCFI", "INVC", "GDPR")),
            isic = 3,
            file = file.path(PATH.REPO, "stanData\\\\data\\\\STANNAi3.rda"),
            replace = FALSE)

## create for icioapp:
packageData(list = c("ICIOVB2013"),
            isic = 3,
            nameyear=c(1995:2009),
            file = file.path(PATH.REPO, "icioData\\\\data\\\\STANICIOi3.rda"),
            replace = TRUE)
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{queryData}}, \code{\link{addDatalist}}
}
\keyword{package}

