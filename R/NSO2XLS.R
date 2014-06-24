#' NSO2XLS
#'
#' Convert NSO data to XLS
#'
#' Convert dataframe for one source to XLS SAS input format.
#'
#' @param data a dataframe object.
#' @param cou a character string 3-digit ISO code specifying the country.
#' @param sou a character string specifying the source.
#' @param cover path to the template Excel workbook containing information about data units.
#' @param file path to the output data file. Default needs to be modified with each update cycle.
#'
#' @author OECD STAN
#' @keywords SAS
#' @seealso \code{\link{ANA2XLS}}
#' @export
#' @examples
#' NSO2XLS(cou="ITA")

NSO2XLS <- function(data=NULL,
                    cou=stop("'cou' must be specified"),
                    sou='NSO',
                    cover=paste0(PATH.COUi4,cou,'/Rawdata/',cou,'_',sou,'_4SAS_template.xls'),
                    file=paste0(PATH.COUi4,cou,'/Rawdata/R_',cou,'_',sou,'_4SAS_2013-14.xls'))
{
    require(XLConnect)
    require(reshape2)
    ##
    sheet <- 'Unites'
    unites <- readWorksheetFromFile(file = cover, sheet = sheet)[,c(1:6)]
    var.load <- unites$VarSTAN
    var.load <- var.load[!is.na(var.load)]
    ##
    if (file.exists(file)==TRUE) file.remove(file)
    wb <- loadWorkbook(filename = file , create = TRUE)
    createSheet(object = wb, name = sheet)
    writeWorksheet(wb, data = unites, sheet = sheet, startRow=1, startCol=1, header=TRUE)
    setColumnWidth(wb, sheet = sheet, column = c(1:6), width = c(2200, 1550, 1800, 15700, 15700, 2800))
    ##
    namevar <- union(intersect(var.load, unique(data$var)), setdiff(unique(data$var), var.load))
    for (var in namevar) {
        data.var <- dcast(data[data$var==var,], ind ~ year, value.var = 'value')
        names(data.var)[2:length(names(data.var))] <- paste0('_',names(data.var)[2:length(names(data.var))])
        createSheet(object = wb, name = var)
        setMissingValue(wb, value = "...")
        writeWorksheet(wb, data = data.var, sheet = var, startRow=1, startCol=1, header=TRUE)
        createFreezePane(wb, sheet = var, colSplit = 2, rowSplit = 2, leftColumn = 2, topRow = 2)
    }
    saveWorkbook(wb)
}
