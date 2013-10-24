\docType{data}
\name{cou}
\alias{cou}
\title{FAME load}
\format{chr "NLD"}
\usage{
  cou
}
\arguments{
  \item{cou}{a 3-digit country ISO code character string.}

  \item{shell}{a character string pointing to the system
  command shell.}

  \item{path.fame}{character string pointing to the
  location of FAME.exe.}

  \item{path.cou}{character string pointing to data
  repository with 3-digit ISO code subfolder.}

  \item{subfolder}{character string pointing to the
  location of the folder conatining GESMES data and FAME
  program file below path.cou.}
}
\description{
  Execute program in FAME
}
\details{
  Start FAME and execute created code file the output is
  written to cou.dat file in the respective countrys
  STD-SNA folder.
}
\examples{
FAMEload(cou="ITA")
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{FAMEall}}, CMD
}
\keyword{datasets}
\keyword{FAME}

