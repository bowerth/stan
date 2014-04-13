#' COU Locate
#'
#' List country files
#'
#' List files and containing folders for a country.
#'
#' @param cou a 3-digit country ISO code character string.
#' @param nameyear a numeric vector with years to search subfolders.
#' @param path a character string specifying the search location.
#'
#' @author OECD STAN
#' @keywords ANA
#' @seealso \code{\link{ANA2XLS}}
#' @export
#' @examples
#' couLocate(cou = 'COL')
#' couLocate(cou = 'COL', getSheets=TRUE)

couLocate <- function(cou=stop("'cou' must be specified"),
                      nameyear=rev(c(2008:2011)),
                      path=PATH.SP,
                      getSheets=FALSE)
{
    files.all <- NULL
    ##
    folders <- list.files(path)
    folder <- folders[substr(folders,1,3)==cou]
    if(!length(folder)==0)
    {
        files <- as.data.frame(list.files(paste0(path, folder)))
        files$folder <- folder
        names(files) <- c('filenames','folder')
        files <- subset(files, select=c('folder', 'filenames'))
        files.all <- rbind(files.all, files)
    }
    for(year in nameyear)
    {
        folders <- list.files(paste0(path, year, '\\'))
        folder <- paste0(folders[substr(folders,1,3)==cou])
        if(!length(folder)==0)
        {
            for(i in seq(along=folder)) {
                files <- as.data.frame(list.files(paste0(path, year, '\\',  folder[i])))
                files$folder <- paste0(year, '\\', folder[i])
                names(files) <- c('filenames','folder')
                files <- subset(files, select=c('folder', 'filenames'))
                files.all <- rbind(files.all, files)
            }
        }
    }
    if (getSheets==FALSE)
    {
        return(files.all)
    } else
    {
        require(XLConnect)
        sheets.all <- NULL
        filepaths <- paste0(files.all[,1], "\\", files.all[,2])
        X <- strsplit(filepaths, "[.]")
        filepaths <- filepaths[sapply(X, '[[', 2)%in%c("xls", "xlsx")]
        for (folder.file in filepaths)
        {
            wb <- loadWorkbook(paste0(path, folder.file))
            sheets <- list(file = getSheets(wb))
            names(sheets) <- folder.file
            sheets.all <- c(sheets.all, sheets)
        }
        return(sheets.all)
    }
}
