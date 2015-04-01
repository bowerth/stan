#' SOU List
#'
#' Create source list
#'
#' Create Source List for countries in folder.
#'
#' @param isic an integer to specify the version of STAN.
#' @param namesou a character string for subfolders containing sources.
#' @param fileext
#'
#' @author OECD STAN
#' @keywords SAS
#' @seealso \code{\link{SASload}}
#' @export
#' @examples
#' souList(isic=3)

souList <- function(isic=4,
                    namesou=c("NSO", "SUT", "STD-SNA"),
                    fileext=c("csv", "txt", "rda")
                    )
{
    if (isic == 3)
    {
        path.cou <- PATH.COUi3
    }
    if (isic == 4)
    {
        path.cou <- PATH.COUi4
    }
    folder.cou <- list.files(path.cou)
    namecou <- folder.cou[nchar(folder.cou)==3 & !folder.cou=="XXX"]
    ##
    list.all <- NULL
    ## c <- match("BRN", namecou)
    for (c in seq(along=namecou))
    {
        cou <- namecou[c]
        cou.sources <- NULL
        ## sou <- "NSO"
        for (sou in namesou)
        {
            if (file.exists(file.path(path.cou, cou, "Rawdata", sou)))
            {
                list.folder <- list.files(file.path(path.cou, cou, "Rawdata", sou))
                ## by matching country code
                X <- strsplit(list.folder, "_")
                undersc <- sapply(X, '[[', 1)
                list.folder.cou <- list.folder[undersc==cou]
                ## by matching file extension
                X <- strsplit(list.folder.cou, "[.]")
                list.folder.cou.fileext <- sapply(X, '[[', 2)
                list.folder.cou.fileext <- list.folder.cou[list.folder.cou.fileext%in%fileext]
                ## by matching source code
                X <- strsplit(sub("[.].+", "", list.folder.cou.fileext), "_")
                list.folder.cou.fileext.sou <- sapply(X, '[[', 2)
                list.folder.cou.fileext.sou <- list.folder.cou.fileext[list.folder.cou.fileext.sou==sou]
                ## by matching source ID
                if (length(list.folder.cou.fileext.sou) > 0)
                {
                    X <- strsplit(list.folder.cou.fileext.sou, "_")
                    filesou <- sapply(strsplit(sapply(X, '[[', 2), "[.]"), '[[', 1)
                    if (length(list.folder.cou.fileext.sou) > 1)
                    {
                        cat(paste0("More than one file found:\n\t", cou, ": ", sou, ": ", toString(list.folder.cou.fileext.sou), "\n"))
                    } else if (length(list.folder.cou.fileext.sou)==1 & filesou==sou)
                    {
                        cou.sources <- c(cou.sources, sou)
                    }
                }
            }
        }
        list.all <- c(list.all, list(cou=cou.sources))
        names(list.all)[c] <- cou
    }
    return(list.all)
}
