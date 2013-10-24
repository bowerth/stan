#' FAME transform
#'
#' Transform FAME output
#'
#' Transform the resulting [cou].dat file to Excel standard format for loading in SAS using \code{\link{ANA2XLS}}.
#'
#' @param cou a 3-digit country ISO code character string.
#' @param isic an integer to specify the version of STAN.
#'
#' @author OECD STAN
#' @keywords FAME
#' @seealso \code{\link{FAMEall}}, \code{\link{ANA2XLS}}
#' @export
#' @examples
#' FAMEloadGES(cou="ITA")

FAMEtransform <- function(cou=stop("'cou' must be specified"),
                          isic=4,
                          append=FALSE)
{
    if (isic==4) path.cou <- PATH.COUi4
    if (isic==3) path.cou <- PATH.COUi3
    path <- paste0(path.cou, cou, '\\', 'Rawdata\\STD-SNA\\')
    ##
    conv.var <- rbind.data.frame(c('L','N','Z','B1G','VALK'),
                                 c('V','N','Z','B1G','VALU'),
                                 c('Y','N','Z','B1G','VKPY'),
                                 ## c('L','N','Z','P1','PRDK'),
                                 c('V','N','Z','P1','PROD'),
                                 ## c('Y','N','Z','P1','PKPY'),
                                 ## c('L','N','Z','P2','INTK'),
                                 c('V','N','Z','P2','INTI'),
                                 ## c('Y','N','Z','P2','IKPY'),
                                 c('V','N','Z','K1','CFCC'),
                                 c('V','N','Z','B2A3N','NOPS'),
                                 c('V','N','Z','D1','LABR'),
                                 c('V','N','Z','D11','WAGE'),
                                 c('V','N','Z','D29X39','OTXS'),
                                 c('L','N','N11','P51','GFCK'),
                                 c('V','N','N11','P51','GFCF'),
                                 c('Y','N','N11','P51','GKPY'),
                                 c('Z','M','Z','EEEM','EMPE'),
                                 c('Z','H','Z','EEEM','HRSE'),
                                 c('Z','M','Z','EETO','EMPN'),
                                 c('Z','H','Z','EETO','HRSN'),
                                 c('Z','M','Z','EESE','SELF'),
                                 c('U','N','T11','Z','CAPN'),
                                 c('O','N','T11','Z','CPNK'),
                                 c('U','N','N11','Z','CAPG'),
                                 c('O','N','N11','Z','CPGK')
                                 )
    names(conv.var) <- c('series.m','series.e','type','var','varSTAN')
    ##
    data <- read.table(paste0(path, tolower(cou), '.dat'), sep = '')
    X <- strsplit(as.character(data[,1]), split = ',')
    X <- X[!sapply(X, '[[', 2)=='NC']
    ##
    data <- cbind.data.frame(as.numeric(sapply(X, '[[', 2)), as.numeric(sapply(X, '[[', 5)))
    names(data) <- c('year','value')
    X <- strsplit(sapply(X,'[[', 1), split = '\\.')
    data$series.m <- sapply(X, '[[', 4) # D
    data$var <- sapply(X, '[[', 5) # E
    data$type <- sapply(X, '[[', 6) # F
    data$s1 <- sapply(X, '[[', 7) # G
    data$ind <- sapply(X, '[[', 8) # H
    data$series.e <- sapply(X, '[[', 11) # K
    data <- subset(data, select=c('series.m', 'series.e', 'var', 's1', 'type', 'ind', 'year', 'value'))
    ## filter certain values
    data <- data[data$s1=='S1',]
    data <- data[data$year >= 1970,]
    ## join STAN variable names
    data <- merge(data, conv.var)
    data <- subset(data, select=c('varSTAN','ind','year','value'))
    names(data) <- sub('varSTAN','var',names(data))
    ##
    ANA2XLS(data = data, cou = cou, append = append)
}
