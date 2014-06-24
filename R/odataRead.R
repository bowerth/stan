#' Query OData OECD.Stat
#'
#' Query data from OECD.Stat using OData
#'
#' Helper function to efficiently query data from OECD.Stat Extracts using SDMX-JSON OData API.
#'
#' @param api an API address
#' @param scheme an API scheme. Available scheme "data".
#' @param DSD a datastructure definition identified by the triplet \code{[collection; country; indicator]}.
#' @param filter for scheme "data": a named list of filters passed to the API. The position of list items corresponds to the API filter dimensions. Each list item is either empty (no filter on dimension) or a character vector containing dimension members to be included in the results. Dimension members can be obtained from \code{scheme="codelist"} and a codelist item, e.g. "CL_ECO_ISIC4".
#' @param query logical to return OData http url only.
#' @param append append string to the dimension url.
#'
#' @author OECD STAN
#' @keywords OECD.Stat, SDMX, OData
#' @seealso \code{\link{ILOSTAT}}, \code{\link{XMLtransform}}, \code{\link{http://stats.oecd.org/OpenDataAPI/OData.html}}
#' @export
#' @examples
#' filter.list <- list(COU = c("ESP", "DEU"),
#'                     FLW = c("IMPO"),
#'                     PAR = c("WOR"),
#'                     EUC = c("TOTAL", "INT"),
#'                     IND = c("DTOTAL", "D01T03"),
#'                     VAL = c("VALUE"))
#' url.append <- paste0("/all?", paste("json-lang=en", "detail=Full", "dimensionAtObservation=AllDimensions", "startPeriod=1990", "endPeriod=2000", sep = "&"))
#' test <- odataRead(DSD = "BTDIXE_I4", filter = filter.list)

odataRead <- function(api="http://stats.oecd.org/OECDStatWCF_OData/OData.svc/",
                      scheme="getmember",
                      DSD="BTDIXE_I4",
                      filter=filter.list,
                      append=url.append,
                      query=FALSE)
{
    require(RCurl)
    require(jsonlite)
    ##
    if (scheme == "getmember") url.scheme <- "GetMember?DatasetCode="
    if (scheme == "getdimension") url.scheme <- "GetDimension?DatasetCode="

    ## theurl <- "http://stats.oecd.org/OECDStatWCF_OData/OData.svc/GetMember?DatasetCode=BTDIXE_I4&$format=json"
    theurl <- paste0(api, url.scheme, DSD, '&$format=json')
    tt <- getURL(theurl)
    data.list2 <- fromJSON(txt = tt)
    ##
    return(data.list2$value)

}
