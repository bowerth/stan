\name{odataRead}
\alias{odataRead}
\title{Query OData OECD.Stat}
\usage{
  odataRead(api = "http://stats.oecd.org/OECDStatWCF_OData/OData.svc/",
    scheme = "getmember", DSD = "BTDIXE_I4",
    filter = filter.list, append = url.append,
    query = FALSE)
}
\arguments{
  \item{api}{an API address}

  \item{scheme}{an API scheme. Available scheme "data".}

  \item{DSD}{a datastructure definition identified by the
  triplet \code{[collection; country; indicator]}.}

  \item{filter}{for scheme "data": a named list of filters
  passed to the API. The position of list items corresponds
  to the API filter dimensions. Each list item is either
  empty (no filter on dimension) or a character vector
  containing dimension members to be included in the
  results. Dimension members can be obtained from
  \code{scheme="codelist"} and a codelist item, e.g.
  "CL_ECO_ISIC4".}

  \item{query}{logical to return OData http url only.}

  \item{append}{append string to the dimension url.}
}
\description{
  Query data from OECD.Stat using OData
}
\details{
  Helper function to efficiently query data from OECD.Stat
  Extracts using SDMX-JSON OData API.
}
\examples{
filter.list <- list(COU = c("ESP", "DEU"),
                    FLW = c("IMPO"),
                    PAR = c("WOR"),
                    EUC = c("TOTAL", "INT"),
                    IND = c("DTOTAL", "D01T03"),
                    VAL = c("VALUE"))
url.append <- paste0("/all?", paste("json-lang=en", "detail=Full", "dimensionAtObservation=AllDimensions", "startPeriod=1990", "endPeriod=2000", sep = "&"))
test <- odataRead(DSD = "BTDIXE_I4", filter = filter.list)
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{ILOSTAT}}, \code{\link{XMLtransform}},
  \code{\link{http://stats.oecd.org/OpenDataAPI/OData.html}}
}
\keyword{OData}
\keyword{OECD.Stat,}
\keyword{SDMX,}

