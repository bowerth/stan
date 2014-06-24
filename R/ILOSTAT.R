#' Query Data ILOSTAT
#'
#' Query data from ILOSTAT SDMX API
#'
#' Helper function to efficiently query data from ILOSTAT SDMX API.
#'
#' @param scheme an API scheme. Available schemes are "datastructure", "codelist" and "data".
#' @param dimension for scheme "datastructure": the format of dimensions returned. The dimension format "codelist" is compatible with scheme "codelist".
#' @param DSD a datastructure definition identified by the triplet \code{[collection; country; indicator]}.
#' @param codelist a codelist as returned by \code{scheme="datastructure", dimension="codelist"}.
#' @param filter for scheme "data": a named list of filters passed to the API. The position of list items corresponds to the API filter dimensions. Each list item is either empty (no filter on dimension) or a character vector containing dimension members to be included in the results. Dimension members can be obtained from \code{scheme="codelist"} and a codelist item, e.g. "CL_ECO_ISIC4".
#' @param query logical to return SDMX http url only.
#'
#' @author OECD STAN
#' @keywords ILO, SDMX
#' @seealso \code{\link{XMLtransform}}
#' @export
#' @examples
#' filter.list <- list(COLLECTION = "YI",
#'                     COUNTRY = c("FRA", "ESP", "AUT"),
#'                     FREQ = NULL,
#'                     SURVEY = NULL,
#'                     REPRESENTED_VARIABLE = NULL,
#'                     CLASSIF_ECO = c("ECO_ISIC4_TOTAL", "ECO_ISIC4_A"),
#'                     CLASSIF_OCU = c("OCU_ISCO08_TOTAL", "OCU_ISCO08_1"))
#'
#' result <- ILOSTAT(scheme="codelist", codelist = "CL_ECO_ISIC4")
#' result <- ILOSTAT(scheme="datastructure", dimension = "codelist", DSD = "YI_ALL_EMP_TEMP_ECO_OCU_NB")
#' result <- ILOSTAT(scheme="datastructure", dimension = "conceptRef", DSD = "YI_ALL_EMP_TEMP_ECO_OCU_NB")
#' result <- ILOSTAT(scheme="data", filter = filter.list, DSD = "YI_ALL_EMP_TEMP_ECO_OCU_NB")

ILOSTAT <- function(scheme="datastructure",
                    ## scheme="codelist",
                    dimension="codelist",
                    ## dimension="conceptRef",
                    DSD="YI_ALL_EMP_TEMP_ECO_OCU_NB",
                    codelist="CL_ECO_ISIC4",
                    filter=filter.list,
                    query=FALSE
                    )
{
    require(XML)
    url.api <- "http://www.ilo.org/ilostat/sdmx/ws/rest/"
    ##
    if (scheme=="datastructure")
    {
        url.scheme <- "datastructure/ILO/"
        theurl <- paste0(url.api, url.scheme, DSD)
        xml.list <- xmlToList(xmlParse(theurl))
        ##
        components.list <- xml.list$KeyFamilies$KeyFamily$Components
        dimensions.list <- components[names(components.list)=="Dimension"]
        ##
        dim.all <- NULL
        for (i in seq(along = dimensions.list))
        {
            dim <- unlist(dimensions.list[i])
            dim <- dim[names(dim)==paste0('Dimension.', dimension)]
            dim.all <- c(dim.all, dim)
            names(dim.all) <- NULL
        }
        return(dim.all)

    } else if (scheme=="codelist")
    {
        url.codelist <- "codelist/ILO/"
        theurl <- paste0(url.api, url.codelist, codelist)
        xml.list <- xmlToList(xmlParse(theurl))
        ##
        codelist <- xml.list$CodeLists$CodeList
        codelist <- codelist[names(codelist)=="Code"]
        ##
        code.all <- NULL
        ## i <- 1
        for (i in seq(along = codelist))
        {
            code <- unlist(codelist[i])
            label <- code[match("en", code)-1]
            code <- code[names(code)=="Code..attrs.value"]
            code.all <- rbind(code.all, c(code, label))
        }
        code.all <- as.data.frame(code.all)
        names(code.all) <- c("code", "label")
        return(code.all)

    } else if (scheme=="data")
    {
        url.scheme <- "data/ILO,DF_"
        ##
        filter.string <- sapply(filter, FUN='toString')
        filter.string <- gsub(", ", "+", filter.string)
        filter.string <- toString(filter.string)
        filter.string <- gsub(", ", ".", filter.string)
        ##
        theurl <- paste0(url.api, url.scheme, DSD, '/', filter.string)
        if (query==TRUE) return(theurl)
        xml.list <- xmlToList(xmlParse(theurl))

        data.all <- NULL
        ## i <- 2
        for (i in seq(along = xml.list$DataSet))
        {

            series <- NULL
            ## series <- matrix(ncol=length(filter))
            ## colnames(series) <- names(filter)
            ## series <- as.data.frame(series)
            ## series <- series[-1,]
            key <- unlist(xml.list$DataSet[i]$Series[1])
            dim <- key[seq(along = names(filter))*2]

            obs.list <- xml.list$DataSet[i]$Series[-1]
            ## j <- 1
            for (j in seq(along = obs.list))
            {
                obs <- unlist(obs.list[j])
                year <- obs[names(obs)=="Obs.Time"]
                value <- obs[names(obs)=="Obs.ObsValue.value"]
                flag <- obs[match("VALUE_STATUS", obs)+1]
                ## row <- data.frame(cou = cou, ind = ind, ocu = ocu, year = year, value = value)
                row <- c(dim, year, value, flag)
                series <- rbind(series, row)
            }
            data.all <- rbind(data.all, series)
        }
        data.df <- as.data.frame(data.all)
        names(data.df) <- c(names(filter), "TIME", "VALUE", "VALUE_STATUS")
        return(data.df)
    }
}
