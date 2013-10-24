\name{queryData}
\alias{queryData}
\title{Query Data SQL}
\usage{
  queryData(connection = stop("'connection' must be specified"),
    table = stop("'table' must be specified"),
    namecou = STAN.COU, namevar = character(),
    dim.ind = "ind", isic = 4, nameind = character(),
    nameyear = character(), add.where = "")
}
\arguments{
  \item{connection}{a connection handle as returned by
  'odbcConnect'.}

  \item{table}{an existing table or view in the database
  specified with \sQuote{connection}.}

  \item{namecou}{a character vector of 3-digit ISO country
  codes read from column \sQuote{cou}.}

  \item{namevar}{a character vector of variables read from
  column \sQuote{var}.}

  \item{dim.ind}{name of column containing the industry
  dimension, if different from \sQuote{ind}.}

  \item{isic}{an integer specifying the ISIC classification
  of data sources.}

  \item{nameind}{a character vector of industries read from
  column \sQuote{ind} or from \sQuote{dim.ind} if
  specified.}

  \item{nameyear}{and integer vector specifying the
  selected period.}

  \item{add.where}{additional statements passed to the
  \sQuote{WHERE} condition of the query.}
}
\description{
  Query data from SQL sources
}
\details{
  Helper function to efficiently query data from SQL
  sources.
}
\examples{
library(RODBC)
SQL.STAN <- odbcDriverConnect(connection="SERVER=VS-GEN-SQL-3;DRIVER=SQL Server;DATABASE=STAN",
                              readOnlyOptimize = TRUE)
DATA.STAN <- queryData(connection=SQL.STAN, table="STANPUB", isic=3)
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{packageData}}, RODBC
}
\keyword{SQL}

