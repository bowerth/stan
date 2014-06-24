#' ANA2XLS
#'
#' Convert ANA data to XLS
#'
#' Convert dataframe for source "ANA" to XLS SAS input format.
#'
#' @param data a dataframe object.
#' @param cou a character string 3-digit ISO code specifying the country.
#' @param isic industry classification to be used for the data output locacation.
#' @param append a boolean expression if variables shall be added to those already present.
#' @param SDMX use SDMX industry list.
#'
#' @author OECD STAN
#' @keywords SAS
#' @seealso \code{\link{NSO2XLS}}
#' @export
#' @examples
#' ANA2XLS(cou="ITA")

ANA2XLS <- function(data=stop("'data' must be specified"),
                    cou=stop("'cou' must be specified"),
                    isic=4,
                    ## path.cou=stop("'path.cou' must be specified"),
                    append=FALSE,
                    SDMX=FALSE)
{
    require(XLConnect)
    require(reshape2)
    ## path.cou: path to COU folder containing country folder.
    if(isic==3)
    {
        path.cou <- PATH.COUi3
    } else if(isic==4)
    {
        path.cou <- PATH.COUi4
    }
    file = file.path(path.cou, cou, "Rawdata", "R_ANA_4SAS_2013-14.xls")
    if (append==FALSE & file.exists(file)==TRUE)
    {
        file.remove(file)
    }
    ## create cover file
    wb <- loadWorkbook(filename = file , create = TRUE)
    sheet <- 'Unites'

    ## produce Windows file paths understood by SAS
    path.mdlind <- gsub("/", "\\\\", file.path(path.cou, cou, "Lists", "MDL_IND_ANA.txt"))
    if (SDMX==TRUE) path.mdlind <- sub("MDL_IND_ANA.txt", "MDL_IND_ANA_SDMX.txt", path.mdlind)
    path.mdlvar <- gsub("/", "\\\\", file.path(path.cou, cou, "Lists", "MDL_VAR_ANA.txt"))

    conv.unit <- rbind.data.frame(c("PROD",6,0),
                                  c("INTI",6,0),
                                  c("VALU",6,0),
                                  c("VALK",6,1),
                                  c("CFCC",6,0),
                                  c("NOPS",6,0),
                                  c("OTXS",6,0),
                                  c("LABR",6,0),
                                  c("WAGE",6,0),
                                  c("GFCF",6,0),
                                  c("GFCK",6,1),
                                  c("EMPN",3,0),
                                  c("EMPE",3,0),
                                  c("HRSN",3,0),
                                  c("HRSE",3,0),
                                  c("JOBN",3,0),
                                  c("CAPN",6,0),
                                  c("CPNK",6,1),
                                  c("CAPG",6,0),
                                  c("CPGK",6,1))
    names(conv.unit) <- c("VarSTAN","Unites","Volume")
    conv.unit$Unites <- as.numeric(as.character(conv.unit$Unites))
    conv.unit$Volume <- as.numeric(as.character(conv.unit$Volume))
    data.cover <- conv.unit[conv.unit$VarSTAN%in%unique(data$var),]
    data.cover$MDLind <- path.mdlind
    data.cover$MDLvar <- path.mdlvar
    data.cover$NOMsource <- "ANA"
    if (nrow(data.cover) > 1)
    {
        data.cover$MDLvar[2:nrow(data.cover)] <- ""
        data.cover$NOMsource[2:nrow(data.cover)] <- ""
    }
    names(data.cover) <- c("VarSTAN","Unites","Volume","MDLind","MDLvar","NOMsource")
    ##
    createSheet(object = wb, name = sheet)
    if (append==TRUE)
    {
        data.cover$MDLvar <- ""
        data.cover$NOMsource <- ""
        data.cover.previous <- readWorksheet(wb, sheet = sheet)
        data.cover <- rbind(data.cover.previous, data.cover)
    }
    writeWorksheet(wb, data = data.cover, sheet = sheet, startRow=1, startCol=1, header=TRUE)
    setColumnWidth(wb, sheet = sheet, column = c(1:6), width = c(2200, 1550, 1800, 16000, 15700, 2800))

    ## write data to worksheets
    var.load <- conv.unit$VarSTAN
    namevar <- union(intersect(var.load, unique(data$var)), setdiff(unique(data$var), var.load))
    for (var in namevar)
    {
        data.var <- dcast(data[data$var==var,], ind ~ year, value.var = "value")
        names(data.var)[2:length(names(data.var))] <- paste0('_',names(data.var)[2:length(names(data.var))])
        createSheet(object = wb, name = var)
        setMissingValue(wb, value = "...")
        writeWorksheet(wb, data = data.var, sheet = var, startRow=1, startCol=1, header=TRUE)
        createFreezePane(wb, sheet = var, colSplit = 2, rowSplit = 2, leftColumn = 2, topRow = 2)
    }
    ## ## Set the data format for numeric columns (cells)
    ## ## (keeping the defaults for all other data types)
    ## setDataFormatForType(wb, type = XLC$"DATA_TYPE.NUMERIC",format = "0.00")
    ## ## Set style action to 'data format only'
    ## setStyleAction(wb, XLC$"STYLE_ACTION.DATA_FORMAT_ONLY")
    saveWorkbook(wb)
}
