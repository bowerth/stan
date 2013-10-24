#' Add Datalist
#'
#' Add R data to package
#'
#' Adding R data files to R packages requires a data list. This is a helper function of \code{\link{loadDim}}.
#'
#' @param file an R data file.
#' @param list a list of objects contained in \sQuote{file}.
#'
#' @author OECD STAN
#' @keywords package
#' @seealso \code{\link{packageData}}
#' @export
#' @examples
#' x <- c(1:10)
#' save(x, file = "data.rda")
#' addDatalist(file = "data.rda", list = c("x"))

addDatalist <- function(file=paste0(PATH.STAN,"data\\stanDim.rda"),
                        list=error("'list' must be specified"))
{
    file.datalist <- paste0(dirname(file), "/datalist")
    if (file.exists(file.datalist)==FALSE)
    {
        file.create(file.datalist)
    }
    if (length(readLines(file.datalist))==0)
    {
        datalist <- NULL
    } else {
        datalist <- read.table(paste0(dirname(file), "/datalist"), sep=":")
        datalist <- datalist[!datalist[,1]==sub("[.].+", "", basename(file)),]
    }
    append <- cbind(sub("[.].+", "", basename(file)), paste0(" ", gsub(",","",toString(list))))
    datalist <- rbind(datalist, append)
    write.table(datalist, paste0(dirname(file), "/datalist"), sep=":", quote=FALSE, row.names=FALSE, col.names=FALSE)
}
