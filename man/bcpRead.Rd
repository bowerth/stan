\name{bcpRead}
\alias{bcpRead}
\title{Bcp read}
\usage{
  bcpRead(table = NULL, server = NULL, bcpOUT = "-w -T")
}
\arguments{
  \item{table}{a character string specifying the SQL table
  in the form [DBname].dbo.[TableName].}

  \item{server}{a character string specifying the SQL
  server containing the database specified with
  \sQuote{table}.}

  \item{bcpOUT}{a character string passing additional
  argumens to the bcp utility for writing data from
  Microsoft SQL Server.}
}
\description{
  Use bcp to read from SQL
}
\details{
  Use the bcp utility to read data from a table on
  Microsoft SQL Server
}
\examples{
bcpRead(table="STAN.dbo.STANPUB", server="VS-GEN-SQL-3")
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{bcpWrite}}
}
\keyword{SQL}

