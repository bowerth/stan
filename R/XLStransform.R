#' XLS transform
#'
#' XLS Questionnaire transformation
#'
#' Transform EU Excel Questionnaire for loading in SAS.
#'
#' @param cou a character string 3-digit ISO code specifying the country.
#' @param isic industry classification to be used for the data output locacation.
#' @param append a boolean expression if variables shall be added to those already present.
#'
#' @author OECD STAN
#' @keywords XLS
#' @seealso \code{\link{ANA2XLS}}
#' @export
#' @examples
#' XLStransform(cou="ITA", isic=4, append=FALSE)

XLStransform <- function(cou=stop("'cou' must be specified"),
                         isic=4,
                         append=FALSE)
{
    require(XLConnect)
    require(reshape2)
    ##
    if (isic==3)
    {
        path <- paste0(PATH.COUi3, cou, '\\Rawdata\\STD-SNA\\')
    } else if (isic==4)
    {
        path <- paste0(PATH.COUi4, cou, '\\Rawdata\\STD-SNA\\')
    }
    ##
    conv.var <- rbind.data.frame(c('V', 'N11', 'S1', 'N', 'P51', 'GFCF'),
                                 c('L', 'N11', 'S1', 'N', 'P51', 'GFCK'),
                                 c('Z', 'Z', 'S1', 'M', 'EETO', 'EMPN'),
                                 c('Z', 'Z', 'S1', 'M', 'EEEM', 'EMPE'),
                                 c('Z', 'Z', 'S1', 'M', 'EESE', 'SELF'),
                                 c('Z', 'Z', 'S1', 'H', 'EETO', 'HRSN'),
                                 c('Z', 'Z', 'S1', 'H', 'EEEM', 'HRSE'),
                                 c('U', 'N11', 'S1', 'N', 'Z', 'CAPG'),
                                 c('O', 'N11', 'S1', 'N', 'Z', 'CPGK'),
                                 c('U', 'T11', 'S1', 'N', 'Z', 'CAPN'),
                                 c('O', 'T11', 'S1', 'N', 'Z', 'CPNK'))
    names(conv.var) <- c('price', 'asset', 'sector', 'denom', 'trans', 'var')
    ##
    filenames <- list.files(path)
    X <- strsplit(x = filenames, split = '[.]')
    filenames.table <- NULL
    for (i in seq(along=filenames)) {
        if (tolower(sapply(X, '[[', 2)[i])%in%c("xls", "xlsx")) {
            filenames.table <- c(filenames.table, filenames[i])
        }
    }
    ##
    data.all <- NULL
    for (file in filenames.table)
    {
        wb <- loadWorkbook(paste0(path, file))
        sheets <- getSheets(wb)
        ##
        for (sheet in sheets)
        {
            meta <- readWorksheet(wb,
                                  sheet = sheet,
                                  region = "A2:D21",
                                  header = FALSE)
            names(meta) <- rep(c('var', 'value'), 2)
            meta <- rbind(meta[,1:2], meta[,3:4])
            meta$var <- tolower(sub(":", "", meta$var))
            ## read column names based on information in metadata
            namedim <- meta$var[!is.na(meta$value) & substr(meta$value, 1, 3)=="Row"]
            ##
            startRowLabel <- min(as.numeric(sub("Row ", "", meta$value[meta$var%in%namedim])))
            endRowLabel <- max(as.numeric(sub("Row ", "", meta$value[meta$var%in%namedim])))
            label <- readWorksheet(wb,
                                   sheet = sheet,
                                   startRow = startRowLabel,
                                   endRow = endRowLabel,
                                   header = FALSE)
            for (i in nrow(label))
            {
                str.dim <- namedim[1]
                str.label <- as.matrix(label[1,])
                if (nrow(label) > 1)
                {
                    str.dim <- paste0(str.dim, "_", as.matrix(namedim[i]))
                    str.label <- paste0(str.label, "_", as.matrix(label[i,]))
                }
            }
            str.label[1] <- "ind"
            ##
            data <- readWorksheet(wb,
                                  sheet = sheet,
                                  startRow = (endRowLabel + 2),
                                  header = FALSE)
            names(data) <- str.label[1:length(data)]
            data.m <- melt(data, id.vars=c("ind"), variable.name=str.dim)
            data.m <- data.m[!is.na(data.m$value),]
            data.m$ind <- gsub(" ", "", data.m$ind)
            ##
            X <- strsplit(as.character(data.m[,colnames(data.m)==str.dim]), split="_")
            for (i in seq(along=namedim))
            {
                if (!namedim[i]%in%names(data.m))
                {
                    data.m <- cbind(data.m, sapply(X, '[[', i))
                    names(data.m)[length(data.m)] <- namedim[i]
                }
            }
            ##
            for (var in c("time", names(conv.var)[!names(conv.var)=="var"]))
            {
                if (!var%in%names(data.m))
                {
                    data.m <- cbind(data.m, meta$value[meta$var==var])
                    names(data.m)[length(data.m)] <- var
                }
            }
            names(data.m) <- sub("time", "year", names(data.m))
            ##
            data1 <- merge(data.m, conv.var)
            data1 <- subset(data1, select = c('var', 'ind', 'year', 'value'))
            ##
            data.all <- rbind(data.all, data1)
        }
    }
    data.out <- data.all
    data.out <- data.out[!is.na(data.out$value),]
    data.out <- data.out[!data.out$value=="L",]
    data.out$value <- as.numeric(data.out$value)
    ## data.d <- dcast(data.all, ind + var ~ year, value.var="value")
    ## FAMEtransform(cou = cou, append = FALSE)
    ANA2XLS(data = data.out, cou = cou, append = append)
}
