#' Load Dimensions
#'
#' Load STAN dimension members
#'
#' Function to create a data file with the dimension members used in STAN.
#'
#' @param dim a character vector of dimension to be loaded.
#' @param file Rdata file where objects will be stored.
#' @param datalist a boolean expression if the file and its contents shall be added to a package data list.
#' @param replace specify if the existing data shall be replaced.
#'
#' @author OECD STAN
#' @keywords dimensions
#' @seealso \code{\link{indAggregate}}
#' @export
#' @examples
#' loadDim(dim = c("cou", "var"), file = "data.rda", datalist = TRUE)

loadDim <- function(dim=c("cou", "var", "indi4", "indi3", "hierarchyi3", "cur"),
                    file=paste0(PATH.REPO,'stan\\data\\stanDim.rda'),
                    datalist=TRUE,
                    replace=FALSE,
                    ...)
{
    if (replace==FALSE)
    {
        env <- new.env()
        load(file, envir = env)
        list <- ls(env)
        load(file)
    } else {
        list <- NULL
    }
    require(ifultools)

    if ("cou"%in%dim)
    {
        require(XLConnect)
        STAN.COU <- readWorksheetFromFile(file = paste0(PATH.SASi4, "lists/STAN_cou_list.xls"), sheet = 1)
        names(STAN.COU) <- tolower(names(STAN.COU))
        STAN.COU2 <- STAN.COU$iso2[STAN.COU$inoecd==1]
        STAN.COU <- STAN.COU$cou[STAN.COU$inoecd==1]
        list <- c(list, "STAN.COU", "STAN.COU2")
    }

    if ("var"%in%dim)
    {
        require(XLConnect)
        STAN.VAR <- readWorksheetFromFile(file = paste0(paste0(PATH.SASi4, "lists/STAN_var_list.xls")), sheet = 1)
        names(STAN.VAR) <- tolower(names(STAN.VAR))
        STAN.VARLABEL  <- STAN.VAR[,colnames(STAN.VAR)%in%c('var','lbvar_en')]
        names(STAN.VARLABEL) <- c('var','label')
        STAN.VARLABEL$label <- properCase(as.character(STAN.VARLABEL$label))
        STAN.VARALL <- as.factor(STAN.VAR$var)
        STAN.VARCLP <- as.factor(STAN.VAR[!is.na(STAN.VAR$clp) & STAN.VAR$clp==1,2])
        STAN.VARPYP <- as.factor(STAN.VAR[!is.na(STAN.VAR$pyp) & STAN.VAR$pyp==1,2])
        STAN.VARMON <- as.factor(STAN.VAR[!is.na(STAN.VAR$mon) & STAN.VAR$mon==1,2])
        STAN.VARPUB <- as.factor(STAN.VAR[!is.na(STAN.VAR$varpub),2])
        list <- c(list, "STAN.VARLABEL", "STAN.VARALL", "STAN.VARCLP", "STAN.VARPYP", "STAN.VARMON", "STAN.VARPUB")
    }

    if ("indi3"%in%dim)
    {
        require(xlsx)                   # necessary if formulas are contained in file
        STANi3.IND <- read.xlsx(paste0(PATH.SASi3,'lists/STAN_Industry_list_i3.xls'),1)
        STANi3.IND <- STANi3.IND[!colnames(STANi3.IND)%in%c('NA.')]
        STANi3.INDLABEL  <- STANi3.IND[,colnames(STANi3.IND)%in%c('Ind','LABEL_en.TEXT')]
        names(STANi3.INDLABEL) <- c('ind','label')
        STANi3.INDLABEL$label <- properCase(as.character(STANi3.INDLABEL$label))
        STANi3.INDALL <- as.factor(STANi3.IND[,"Ind"])

        STANi3.INDA6 <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndA6) & STANi3.IND$IndA6==1,3])
        STANi3.INDA17 <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndA17) & STANi3.IND$IndA17==1,3])
        STANi3.INDA31 <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndA31) & STANi3.IND$IndA31==1,3])
        STANi3.INDA60 <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndA60) & STANi3.IND$IndA60==1,3])
        STANi3.INDICIO <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndICIO),3])
        ##
        STANi3.INDA18 <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndA18),3])
        STANi3.INDA18 <- STANi3.INDA18[order(STANi3.INDA18)]
        ##
        STANi3.INDA34 <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndA34),3])
        STANi3.INDA34 <- STANi3.INDA34[order(STANi3.INDA34)]
        ## ## one list with all industries + aggregates for level sorting
        ## STANi3.INDA60All <- cbind.data.frame(STANi3.IND$Ind, STANi3.IND$IndA60All)
        ## STANi3.INDA60All <- STANi3.INDA60All[order(STANi3.INDA60All[,2]),]
        ## STANi3.INDA60All <- as.factor(STANi3.INDA60All[!is.na(STANi3.INDA60All[,2]),1])
        ## STANi3.INDA60All <- factor(STANi3.INDA60All, levels = STANi3.INDA60All)
        detach("package:xlsx", unload=TRUE)
        list <- c(list, "STANi3.INDLABEL", "STANi3.INDALL", "STANi3.INDA6", "STANi3.INDA17", "STANi3.INDA31", "STANi3.INDA60", "STANi3.INDICIO", "STANi3.INDA18", "STANi3.INDA34")
    }

    if ("hierarchyi3"%in%dim)
    {
        matrix.agg <- paste0(PATH.COUi3, 'Aggregation_general_2digit_ISIC3.csv')
        nameagg <- read.csv(matrix.agg)[,1]
        agg.include <- union(union(union(STANi3.INDA6, STANi3.INDA18), STANi3.INDA34), c("CTOTAL", "C15T37", "C50T74", "C99"))
        agg.include <- union(agg.include, "C65T99") # UNSD SNA
        agg.include <- union(agg.include, "C80T93") # UNData M+N+O
        ## cat(paste0(sort(agg.include), "\n"))
        agg.exclude <- setdiff(nameagg, agg.include)
        ## agg.exclude <- c("C20A36", "C10T41", "C27T35", "C50T74X", "C10T74X", "LOTECH", "HMHTECH", "ENERGYP", "NONMAN")
        ## agg.exclude <- c(agg.exclude, "C50T64", "C65T74", "C50T74", "C50T99")
        ## include "C65T99", "HITECH", "ICTMAN", "ICTSER", "MHTECH", "MLTECH"
        STANi3.HIERARCHY <- hierarchy(file = matrix.agg, agg.exclude = agg.exclude)
        STANi3.HIERARCHYINV <- hierarchy(file = matrix.agg, agg.exclude = agg.exclude, parent = TRUE)
        STANi3.INDA60All <- hierarchy(file = matrix.agg, agg.exclude = agg.exclude, order = TRUE)
        STANi3.INDA60All <- STANi3.INDA60All[STANi3.INDA60All%in%agg.include]
        list <- c(list, "STANi3.HIERARCHY", "STANi3.HIERARCHYINV", "STANi3.INDA60All")
    }

    if ("indi4"%in%dim)
    {
        require(xlsx)                   # necessary if formulas are contained in file
        ## STAN.IND <- readWorksheetFromFile(file = paste0(PATH.SASi4,'lists/STAN_Industry_list_i4.xls'), sheet = 2)
        STANi4.IND <- read.xlsx(paste0(PATH.SASi4, "lists/STAN_Industry_list_i4.xls"),2)
        STANi4.IND <- STANi4.IND[!colnames(STANi4.IND)%in%c('NA.')]
        STANi4.INDLABEL  <- STANi4.IND[,colnames(STANi4.IND)%in%c('Ind','LABEL_en.TEXT')]
        names(STANi4.INDLABEL) <- c('ind','label')
        STANi4.INDLABEL$label <- properCase(as.character(STANi4.INDLABEL$label))
        STANi4.INDALL <- as.character(STANi4.IND[,3][!is.na(STANi4.IND[,3])])
        STANi4.INDA10 <- as.character(STANi4.IND[!is.na(STANi4.IND$IndA10) & STANi4.IND$IndA10==1,3])
        STANi4.INDA21 <- as.character(STANi4.IND[!is.na(STANi4.IND$IndA21) & STANi4.IND$IndA21==1,3])
        STANi4.INDA38 <- as.character(STANi4.IND[!is.na(STANi4.IND$IndA38) & STANi4.IND$IndA38==1,3])
        STANi4.INDA64 <- as.character(STANi4.IND[!is.na(STANi4.IND$IndA64) & STANi4.IND$IndA64==1,3])
        STANi4.INDA88 <- as.character(STANi4.IND[!is.na(STANi4.IND$IndA88) & STANi4.IND$IndA88==1,3])
        STANi4.INDICIO <- as.character(STANi4.IND[!is.na(STANi4.IND$IndICIO),3])
        detach("package:xlsx", unload=TRUE)
        list <- c(list, "STANi4.INDLABEL", "STANi4.INDALL", "STANi4.INDA10", "STANi4.INDA21", "STANi4.INDA38", "STANi4.INDA64", "STANi4.INDA88", "STANi4.INDICIO")
    }

    if ("hierarchyi4"%in%dim)
    {
        matrix.agg <- paste0(PATH.COUi4, 'Aggregation_general_2digit.csv')
        agg.exclude <- c("D05T39", "D45T82", "D45T99", "D45T82X", "D05T82X", "D16A31", "D24T33X", "ENERGYP", "NONMAN")
        ## include "C10T41", "C65T99", "HITECH", "ICTMAN", "ICTSER", "MHTECH", "MLTECH"
        ## included: "C10T41", "C50T64", "C65T74", "C50T74", "C50T99"
        STANi4.HIERARCHY <- hierarchy(file = matrix.agg, agg.exclude = agg.exclude)
        STANi4.HIERARCHYINV <- hierarchy(file = matrix.agg, agg.exclude = agg.exclude, parent = TRUE)
        list <- c(list, "STANi4.HIERARCHY", "STANi4.HIERARCHYINV")
    }

    if ("cur"%in%dim)
    {
        require(XML)
        require(XLConnect)
        ## United Nations, Countries or areas, codes and abbreviations
        url.UN <- rbind.data.frame(c("en", "http://unstats.un.org/unsd/methods/m49/m49alpha.htm"),
                                   c("fr", "http://unstats.un.org/unsd/methods/m49/m49alphaf.htm"))
        ##
        table.UN.all <- NULL
        for (i in seq(along=url.UN))
        {
            table.UN <- readHTMLTable(as.character(url.UN[i,2]), encoding = "utf-8")
            table.UN <- table.UN[[4]]
            table.UN <- table.UN[-c(1:19),]
            names(table.UN) <- c("codeUN", "countryUN", "cou")
            table.UN <- table.UN[length(table.UN[,1]) > 3 & !is.na(table.UN[,3]),]
            table.UN$countryUN <- gsub("\r\n", " ", table.UN$countryUN)
            table.UN$countryUN <- gsub("[ ]+", " ", table.UN$countryUN)
            table.UN$lang <- url.UN[i,1]
            table.UN.all <- rbind(table.UN.all, table.UN)
        }
        ##
        table.UN <- (merge(table.UN.all[table.UN.all$lang=="en",],
                           table.UN.all[table.UN.all$lang=="fr",], by = "cou"))
        names(table.UN) <- sub("countryUN.x", "countryUNen", names(table.UN))
        names(table.UN) <- sub("countryUN.y", "countryUNfr", names(table.UN))
        names(table.UN) <- sub("codeUN.x", "codeUN", names(table.UN))
        table.UN <- subset(table.UN, select = c("cou", "countryUNen", "countryUNfr", "codeUN"))
        table.UN <- table.UN[!table.UN$cou=="",]
        STAN.COUUN <- subset(table.UN, select = c("cou", "codeUN"))
        ## nationsonline.org
        url.NO <- "http://www.nationsonline.org/oneworld/country_code_list.htm"
        table.NO <- readHTMLTable(url.NO)
        table.NO <- table.NO[[4]]
        table.NO <- table.NO[-1,]
        names(table.NO) <- c("flagNO", "countryNO", "cou2", "cou", "codeNO")
        ## merge UN and NO
        table.UN.NO <- merge(table.UN, table.NO)
        ## ISO 3166 country names (for check) - currently not available 02/10/2104
        url.ISO3166 <- paste0(PATH.SASi3, "DATA_in\\ISO\\country_names_and_code_elements_txt-temp.txt")
        ## url.ISO3166 <- "http://www.iso.org/iso/home/standards/country_codes/country_names_and_code_elements_txt.htm"
        table.ISO3166 <- readLines(url.ISO3166, encoding = "UTF-8")
        ##
        X <- strsplit(table.ISO3166, split = ";")
        X <- X[sapply(X, length) > 0]
        table.ISO3166 <- matrix("", nrow=length(X), ncol=2)
        for (i in seq(along=X))
        {
            table.ISO3166[i,1] <- X[[i]][1]
            table.ISO3166[i,2] <- X[[i]][2]
        }
        table.ISO3166 <- as.data.frame(table.ISO3166)
        table.ISO3166 <- table.ISO3166[-1,]
        names(table.ISO3166) <- c("countryISO", "cou2")
        table.UN.NO.ISO3166 <- merge(table.UN.NO, table.ISO3166)
        ## ISO 4217 from xls link
        file.ISO4217 <- tempfile(fileext = ".xls")
        ##
        download.file("http://www.currency-iso.org/dam/downloads/table_a1.xls", destfile = file.ISO4217, method = 'curl')
        ##
        table.ISO4217 <- readWorksheetFromFile(file.ISO4217, sheet = 1, startRow = 4)
        names(table.ISO4217) <- c("countryISO", "currency", "cur", "curcode", "unitISO", "fundISO")
        table.UN.NO.ISO3166.ISO4217 <- merge(table.UN.NO.ISO3166, table.ISO4217)
        ## Eurostat names, codes and protocol order
        require(RCurl)
        url.EU <- "http://publications.europa.eu/code/pdf/370000en.htm"
        curl <- getCurlHandle()
        agent <- "Mozilla/5.0"
        curlSetOpt(cookiejar="cookiesk.txt",  useragent = agent, followlocation = TRUE, curl=curl)
        tt <- getURL(url.EU, curl = curl)
        table.EU <- readHTMLTable(tt, encoding = "utf-8")
        table.EU <- table.EU[[3]]
        table.EU <- table.EU[-1,]
        names(table.EU) <- c("countryEUnat", "countryEUen", "countryEUoff", "cou2EU", "former")
        table.EU$cou2 <- table.EU$cou2EU
        table.EU$cou2 <- sub("EL", "GR", table.EU$cou2)
        table.EU$cou2 <- sub("UK", "GB", table.EU$cou2)
        table.EU$countryEUen <- gsub("\r\n", "", table.EU$countryEUen)
        table.EU$countryEUoff <- gsub("\r\n", "", table.EU$countryEUoff)
        table.EU <- table.EU[,!colnames(table.EU)%in%c("countryEUnat", "former")]
        table.EU <- table.EU[!is.na(table.EU$cou2EU),]
        table.EU$inEURO <- 1
        ##
        table.UN.NO.ISO3166.ISO4217.EU <- merge(table.UN.NO.ISO3166.ISO4217, table.EU, by = c("cou2"), all = TRUE)
        ##
        table <- table.UN.NO.ISO3166.ISO4217.EU
        table$inEURO[is.na(table$inEURO)] <- 0
        ##
        STAN.CUR <- subset(table[is.na(table$fundISO) & !is.na(table$cur),], select = c("cou", "cur"))
        STAN.CUR$cou <- as.character(STAN.CUR$cou)
        STAN.CUR <- rbind.data.frame(STAN.CUR, c("TWN", "TWD"))
        STAN.CUR$cou <- as.factor(STAN.CUR$cou)
        ##
        STAN.COUEN <- subset(table[is.na(table$fundISO) & !is.na(table$cur),], select = c("cou", "countryUNen"))
        STAN.COUEN$countryUNen <- gsub(" (Plurinational State of)", "", STAN.COUEN$countryUNen, fixed = TRUE) # Bolivia
        STAN.COUEN$countryUNen <- gsub(" (Bolivarian Republic of)", "", STAN.COUEN$countryUNen, fixed = TRUE) # Venezuela
        STAN.COUEN$countryUNen <- gsub(" (Islamic Republic of)", "", STAN.COUEN$countryUNen, fixed = TRUE) # Iran
        STAN.COUEN$cou <- as.character(STAN.COUEN$cou)
        STAN.COUEN <- rbind.data.frame(STAN.COUEN, c("TWN", "Chinese Taipei"))
        STAN.COUEN$cou <- as.factor(STAN.COUEN$cou)
        ##
        STAN.COUFR <- subset(table[is.na(table$fundISO) & !is.na(table$cur),], select = c("cou", "countryUNfr"))
        STAN.COUFR$cou <- as.character(STAN.COUFR$cou)
        STAN.COUFR <- rbind.data.frame(STAN.COUFR, c("TWN", "Taipei Chinois"))
        STAN.COUFR$cou <- as.factor(STAN.COUFR$cou)
        ##
        STAN.COU2 <- subset(table[is.na(table$fundISO) & !is.na(table$cur),], select = c("cou", "cou2"))
        STAN.COUEU <- subset(table[table$inEURO==1,], select = c("cou"))[,1]
        ## STAN.COUUN <- subset(table[is.na(table$fundISO) & !is.na(table$cur),], select = c("cou", "codeUN")) # see above
        list <- c(list, "STAN.CUR", "STAN.COUEN", "STAN.COUFR", "STAN.COU2", "STAN.COUEU", "STAN.COUUN")
    }
    save(list = list, file = file)
    if (datalist==TRUE) addDatalist(file = file, list = list)
}
