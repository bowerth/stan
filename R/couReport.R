#' COU Report
#'
#' Create National Source Report
#'
#' Creates HTML report for a flat data file.
#'
#' @param cou.sou a 3-digit country ISO code character string and source ID.
#' @param brew.file the name of the brew file located a character string prepending the output filename.
#' @param isic an integer specifying the version of ISIC.
#' @param create.data logical controls creation of  SAS input data.
#' @param create.plot logical controls creation of data plot with all sources.
#' @param namevar.plot a character vector specifying the variables to be plotted.
#' @param file.brew a character string specifying the brew file to be used.
#' @param file.out.prefix a character string implicitly specifying the output location and filename.
#' @param date a character string specifying the collection period in the output filename.
#'
#' @author OECD STAN
#' @keywords SAS
#' @seealso \code{\link{NSO2XLS}}
#' @export
#' @examples
#' couReport(cou="FRA")
#'
#' namecou <- c('BEL', 'DEU', 'FIN', 'FRA', 'ITA', 'NLD')
#' seeds <- as.character(namecou)
#' path <- paste0(sub("Progs", "Data", PATH.COUi3))
#' results <- sapply(seeds, couReport,
#'                   brew.file = paste0(path, "STAN_collect_load.brew"),
#'                   path.out = paste0(path, cou, "\\Rawdata\\Report_"))
#' for (url in results) browseURL(url)

couReport <- function(cou.sou=stop("'cou.sou' must be specified"),
                      isic=4,
                      create.data=TRUE,
                      create.plot=TRUE,
                      namevar.plot=NULL,
                      file.brew=paste0(PATH.COUi4, "STAN_collect_load.brew"),
                      file.out.prefix="\\Rawdata\\Report_",
                      date=2013)
{
    require(brew)
    require(knitr)
    require(screening)
    require(reshape2)
    if (isic==3)
    {
        path.cou <- PATH.COUi3
    }
    if (isic==4)
    {
        path.cou <- PATH.COUi4
    }
    cou <- sapply(strsplit(cou.sou, "_"), "[[", 1)
    sou <- sapply(strsplit(cou.sou, "_"), "[[", 2)
    ##
    path.out <-  paste0(path.cou, cou, file.out.prefix)
    ##
    rhtml.file <- paste0(path.out, cou.sou, date, ".Rhtml")
    brew(file.brew, rhtml.file) # This generates the actual rhtml file for different seeds
    knit(input=rhtml.file,
         output=paste0(path.out, cou.sou, date, ".html"))
    out.file <- paste0(path.out, cou.sou, date, ".html")
    return(out.file)
}
