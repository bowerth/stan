#' Access .Stat SQL
#'
#' Use stored procedures to retrieve from OECD.Stat
#'
#' \code{dotStatGetData} : retrieve data based on dimension member list
#'
#' @param channel open SQL channel created with \code{odbcDriverConnect}
#' @param datasetcode character code of dataset
#' @param lang character \code{en} or \code{fr}
#' @param dim character to specify dimension
#' @param dim.list named list with character vector for each dimension in dataset
#' @param time integer vector with start and end year
#'
#' @author OECD STAN
#' @keywords SDBS
#' @export
#' @examples
#' ## create the dimension list with members - order of dimensions as in dataset
#' dim.list.test <- list(LOCATION=c("AUT", "BEL"),
#'                       VAR=c("PROD", "VALU"),
#'                       ISIC4=c("05_09", "10_33"),
#'                       SRC=c("SSIS"),
#'                       ## SRC=c(""),
#'                       SCL=c("TOTAL"))
#' ## retrieve data according to selected dimension member selection
#' data.test <- dotstatGetData(channel = SQL.STAT,
#'                             datasetcode = "SSIS_BSC_ISIC4",
#'                             dim.list = dim.list.test,
#'                             time = c(2007, 2008))
dotstatGetData <- function(channel,
                           datasetcode,
                           dim.list,
                           time) {
    query1 <- sapply(dim.list, function(x) gsub(", ", "+", toString(x)))
    query1 <- paste(query1, collapse = ".")
    query2 <- paste0('OECD?StartTime=', time[1], '&EndTime=', time[2])
    query <- paste0('exec get.data \'', paste(datasetcode, query1, query2, sep = '/'), '\'')
    data <- sqlQuery(channel = channel, query)
    names(data) <- sub("_TIME_", "TIME", names(data))
    data <- subset(data, select = c(names(dim.list), "TIME", "VALUE"))
    return(data)
}

#' dotStatGetDimensionList
#'
#' \code{dotStatGetDimensionList}: Retrieve dimensions of dataset.
#'
#' dotStatGetDimensionList(channel, datasetcode, lang)
#'
#' @rdname dotstatGetData
#' @export
#' @examples
#' namedim <- dotStatGetDimensionList(channel = SQL.STAT,
#'                                    datasetcode = "SSIS_BSC_ISIC4",
#'                                    lang = "en")
dotStatGetDimensionList <- function(channel,
                                    datasetcode,
                                    lang) {
    query <- paste0('exec get.dimensionList \'', datasetcode, '\', \'', lang, '\'')
    dim <- sqlQuery(channel = channel, query = query)
    dim <- dim$Code[dim$IsTimeSeriesDim!=1]
    dim <- as.character(dim)
    return(dim)
}

#' dotStatGetDimensionMemberList
#'
#' \code{dotStatGetDimensionMemberList}: Retrieve dimension members of dataset.
#'
#' dotStatGetDimensionMemberList(dim, channel, datasetcode, lang)
#'
#' @rdname dotstatGetData
#' @export
#' @examples
#' ## for a single dimension
#' dotStatGetDimensionMemberList(dim = "LOCATION",
#'                               channel = SQL.STAT,
#'                               datasetcode = "SSIS_BSC_ISIC4",
#'                               lang = "en")
#' ## for multiple dimensions
#' namedim <- c("LOCATION", "VAR")
#' dim.list <- lapply(namedim, dotStatGetDimensionMemberList,
#'                    channel = SQL.STAT,
#'                    datasetcode = "SSIS_BSC_ISIC4",
#'                    lang = "en")
#' names(dim.list) <- namedim
dotStatGetDimensionMemberList <- function(dim,
                                          channel,
                                          datasetcode,
                                          lang) {
    query <- paste0('exec get.dimensionMemberList \'', datasetcode, '\', \'', dim, '\', \'', lang, '\'')
    dim.member <- sqlQuery(channel = channel, query = query)
    dim.member <- as.character(dim.member$Code)
    return(dim.member)
}
