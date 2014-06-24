#' XML transform (EU)
#'
#' XML Questionnaire transformation (EU)
#'
#' Transform EU SDMX Questionnaire for loading in SAS.
#'
#' @param cou a character string 3-digit ISO code specifying the country.
#' @param isic industry classification to be used for the data output locacation.
#' @param append a boolean expression if variables shall be added to those already present.
#' @param SDMX use SDMX industry list.
#'
#' @author OECD STAN
#' @keywords XML
#' @seealso \code{\link{ANA2XLS}}
#' @export
#' @examples
#' XMLtransform(cou = "POL", isic = 4, append = FALSE, SDMX = TRUE)

## XMLtransform(cou = "SWE", isic = 4, append = FALSE, SDMX = TRUE)

XMLtransform <- function(cou=stop("'cou' must be specified"),
                         isic=4,
                         append=FALSE,
                         SDMX=FALSE)
{
    require(XML)
    require(reshape2)
    ##
    if (isic==3)
    {
        path.cou <- paste0(PATH.COUi3, cou, '\\Rawdata\\STD-SNA\\')
    } else if (isic==4)
    {
        path.cou <- paste0(PATH.COUi4, cou, '\\Rawdata\\STD-SNA\\')
    }
    filenames <- list.files(path.cou)
    X <- strsplit(x = filenames, split = '[.]')
    filenames.table <- NULL
    for (i in seq(along=filenames)) {
        if (tolower(sapply(X, '[[', 2)[i])%in%c("xml")) {
            filenames.table <- c(filenames.table, filenames[i])
        }
    }
    conv.var <- rbind.data.frame(c("L","XDC","_Z","B1G","VALK"),
                                 c("V","XDC","_Z","B1G","VALU"),
                                 c("Y","XDC","_Z","B1G","VKPY"),
                                 ## c("L","XDC","_Z","P1","PRDK"),
                                 c("V","XDC","_Z","P1","PROD"),
                                 ## c("Y","XDC","_Z","P1","PKPY"),
                                 ## c("L","XDC","_Z","P2","INTK"),
                                 c("V","XDC","_Z","P2","INTI"),
                                 ## c("Y","XDC","_Z","P2","IKPY"),
                                 c("V","XDC","_Z","P51C","CFCC"),
                                 c("V","XDC","_Z","B2A3N","NOPS"),
                                 c("V","XDC","_Z","D1","LABR"),
                                 c("V","XDC","_Z","D11","WAGE"),
                                 c("V","XDC","_Z","D29X39","OTXS"),
                                 c("L","XDC","N11G","P51G","GFCK"),
                                 c("V","XDC","N11G","P51G","GFCF"),
                                 c("Y","XDC","N11G","P51G","GKPY"),
                                 c("_Z","PS","_Z","SAL","EMPE"),
                                 c("_Z","HW","_Z","SAL","HRSE"),
                                 c("_Z","PS","_Z","EMP","EMPN"),
                                 c("_Z","HW","_Z","EMP","HRSN"),
                                 c("_Z","PS","_Z","SELF","SELF"),
                                 c("U","XDC","N11N","LE","CAPN"),
                                 c("O","XDC","N11N","LE","CPNK"),
                                 c("U","XDC","N11G","LE","CAPG"),
                                 c("O","XDC","N11G","LE","CPGK")
                                 )
    names(conv.var) <- c("prices", "unit_measure", "instr_asset", "sto", "var")
    cat("Initiate loading...\n")
    data.all <- NULL
    for (file in filenames.table)
    {
        cat("\t", file, "\n")
        xml.data <- xmlParse(file = paste0(path.cou, file))
        xml.list <- xmlToList(xml.data)
        data.list <- xml.list[2]
        data.file <- NULL
        for (i in seq(along = data.list$DataSet[names(data.list$DataSet)=="Series"]))
        {
            data.obs <- data.list$DataSet[[i]][names(data.list$DataSet[[i]])=="Obs"]
            data.attrs <- data.list$DataSet[[i]][names(data.list$DataSet[[i]])==".attrs"][[1]]
            namevar <- names(data.attrs)[!names(data.attrs)%in%c("REF_YEAR_PRICE", "LAST_UPDATE")]
            ##
            data <- data.frame(year = sapply(data.obs, "[[", 1), # year
                               value = sapply(data.obs, "[[", 2) # value
                               )
            ## include all variables
            for (var in namevar)
                ## for (name in c("ACTIVITY", "STO", "PRICES", "UNIT_MEASURE"))
            {
                eval(parse(text = paste0('data$', tolower(var), ' <- data.attrs[names(data.attrs)=="', var, '"]')))
            }
            data.file <- rbind(data.file, data)
            ## unique(data.file$instr_asset)
            ## unique(data.file$prices)
            ## h(data.file)
        }
        data.all <- rbind(data.all, data.file)
    }
    data.out <- data.all
    data.out$year <- as.numeric(as.character(data.out$year))
    data.out$value <- as.numeric(as.character(data.out$value))
    data.out <- data.out[!is.na(data.out$value),]
    data.out <- merge(data.out, conv.var)
    names(data.out) <- sub("activity", "ind", names(data.out))
    data.out <- subset(data.out, select = c("var", "ind", "year", "value"))
    cat("Write to XLS...\n")
    ANA2XLS(data = data.out, cou = cou, isic = isic, append = append, SDMX = SDMX)
    cat("Done!\n")
}

## ## contents of variables
## for (var in namevar)
## {
##     eval(parse(text = paste0('cat("', var, '", "\n", paste0("\t", unique(data.file$', tolower(var), '), "\n"))')))
## }
##
## ## create column string for pivoting
## id.var <- names(data.file)[!names(data.file)%in%c("year", "value")]
## gsub(",", "", toString(paste(id.var, "+")))
## ##
## data.file.d <- dcast(data.file, freq + adjustment + ref_area + counterpart_area + ref_sector + counterpart_sector + accounting_entry + sto + instr_asset + activity + expenditure + unit_measure + prices + transformation + ref_period_detail + time_format + decimals + table_identifier + unit_mult ~ year, value.var = "value")
## ##
## ## check for duplicates
## data.check <- data.file[duplicated(data.file[,!colnames(data.file)%in%c("value")]),]
## eval(parse(text = paste0('data.check <- data.check[order(', toString(paste0('data.check$', names(data.check)[!names(data.check)=="value"])), '),]')))
## ##
## data.check.d <- dcast(data.check, freq + adjustment + ref_area + counterpart_area + ref_sector + counterpart_sector + accounting_entry + sto + instr_asset + activity + expenditure + unit_measure + prices + transformation + ref_period_detail + time_format + decimals + table_identifier + unit_mult ~ year, value.var = "value")
