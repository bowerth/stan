#' XLS transform (non-EU)
#'
#' XLS Questionnaire transformation (non-EU)
#'
#' Transform non-EU Excel Questionnaire for loading in SAS.
#'
#' @param cou a 3-digit country ISO code character string.
#' @param isic industry classification to be used for the data output locacation.
#' @param append a boolean expression if variables shall be added to those already present.
#' @param path a character string specifying the search location.
#' @param folder a character string specifying the containing folder.
#' @param file a character string specifying the file name.
#' @param prefix a character string specifying the sheet prefix.
#' @param nameyear a numeric vector with years in worksheet names.
#' @param startRow an integer specifying the first data row.
#' @param startCol an integer specifying the first data column.
#' @param endCol an integer specifying the last data column.
#' @param colNames an integer with variable names.
#'
#' @author OECD STAN
#' @keywords ANA
#' @seealso \code{\link{couLocate}}, \code{\link{ANA2XLS}}
#' @export
#' @examples
#' cou <- "COL"
#' folder.file <- couLocate(cou = cou)[3,] # select the file here
#' folder <- as.character(folder.file[,1])
#' file <- as.character(folder.file[,2])
#' XLStransform2(cou="COL", isic = 3, folder=folder, file=file, prefix="3.1_", nameyear=c(2000:2011))
#' cou <- "MEX"
#' folder.file <- couLocate(cou = cou)[1,] # select the file here
#' folder <- as.character(folder.file[,1])
#' file <- as.character(folder.file[,2])
#' XLStransform2(cou="MEX", isic = 3, folder=folder, file=file, prefix="3.1_", nameyear=sprintf("%02d", c(3:11)))

XLStransform2 <- function(cou=stop("'cou' needs to be specified"),
                          isic=4,
                          append=FALSE,
                          path=PATH.SP,
                          folder=stop("'folder' needs to be specified"),
                          file=stop("'file' needs to be specified"),
                          prefix="",
                          nameyear=c(2000:2011),
                          startRow=14,
                          startCol=1,
                          endCol=11,
                          colNames=c('ind','PROD','INTI','VALU','VALK','CFCC','CFCK','NOPS','OTXS','LABR','WAGE'))
{
    require(XLConnect)
    require(reshape2)
    if (isic==3)
    {
        path.cou <- paste0(PATH.COUi3, cou, '\\Rawdata\\STD-SNA\\')
    } else if (isic==4)
    {
        path.cou <- paste0(PATH.COUi4, cou, '\\Rawdata\\STD-SNA\\')
    }
    ##
    removeComma= function(s) {gsub(",", "", s, fixed = TRUE)}
    ##
    data.all <- NULL
    wb <- loadWorkbook(filename = paste0(path, folder, "\\", file))
    for(year in nameyear) {
        sheet <- paste0(prefix, year)
        data <- readWorksheet(object = wb, sheet = sheet,
                              startRow = startRow, startCol = startCol,
                              endCol = endCol, header = FALSE)
        data <- as.data.frame(apply(data, 2, removeComma))
        colnames(data) <- colNames[1:length(data)]
        data.m <- melt(data, id.vars=c('ind'), variable.name='var', na.rm=TRUE)
        data.m$ind <- gsub(" ", "", data.m$ind)
        data.m$ind <- sub("-", "_", data.m$ind)
        if (nchar(year)==2) year <- paste0("20", year)
        data.m$year <- as.numeric(as.character(year))
        data.m$value <- as.numeric(data.m$value)
        data.all <- rbind(data.all, data.m)
    }
    data.out <- data.all
    data.out <- data.out[!is.na(data.out$value),]
    data.out <- data.out[!is.na(data.out$ind),]
    write.csv(data.out, file = paste0(path.cou, cou, "_STD-SNA.csv"), row.names = FALSE)
    ANA2XLS(data = data.out, cou = cou, isic = isic, append = append)
}
