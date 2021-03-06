% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bcpWrite.R
\name{bcpWrite}
\alias{bcpWrite}
\title{Bcp write}
\usage{
bcpWrite(x = NULL, table = NULL, server = NULL, bcpIN = "-w -T")
}
\arguments{
\item{x}{a dataframe object with the data to be written to SQL.}

\item{table}{a character string specifying the SQL table in the form [DBname].dbo.[TableName].}

\item{server}{a character string specifying the SQL server containing the database specified with \sQuote{table}.}

\item{bcpIN}{a character string passing additional argumens to the bcp utility for reading data into Microsoft SQL Server.}
}
\description{
Use bcp to write to SQL
}
\details{
Use the bcp utility to write data to a table on Microsoft SQL Server
}
\examples{
x <- data.frame("values"=c(1:10))
bcpWrite(x=x, table="STAN.dbo.STANPUB", server="VS-GEN-SQL-3")
}
\author{
OECD STAN
}
\seealso{
\code{\link{bcpRead}}
}
\keyword{SQL}

