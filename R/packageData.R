#' Package Data
#'
#' Package National Accounts data sources
#'
#' This function prepares R data files for packaging. It is primarily intended to maintain data sources from National Accounts (in million USD).
#'
#' @param list a character vector of National Accounts data source IDs.
#' @param isic an integer specifying the ISIC classification of data sources.
#' @param namecou a character vector of 3-digit ISO country codes read from column \sQuote{cou}.
#' @param namepar a character vector of 3-digit ISO partner codes read from column \sQuote{par}.
#' @param namevar a character vector of variables read from column \sQuote{var}.
#' @param nameeuc a character vector of end-use codes read from column \sQuote{euc}.
#' @param nameind a character vector of industries read from column \sQuote{ind} or from \sQuote{dim.ind} if specified.
#' @param nameyear and integer vector specifying the selected period.
#' @param replace specify if the existing data shall be replaced.
#' @param sqlite specify if the data shall be stored as SQLite database.
#'
#' @author OECD STAN
#' @keywords package
#' @seealso \code{\link{queryData}}, \code{\link{addDatalist}}
#' @export
#' @examples
#' ## create for stanapp:
#' sourcesSTANNAi3 <- c('UNSDSNA2013', 'ICIO052013', 'WIOT042012', 'STAN', 'BTD')
#' packageData(list=sourcesSTANNAi3,
#'             namecou = unique(c(union(STAN.COU, STAN.COUKPC), "BGR", "BRN", "CYP", "HKG", "KHM", "LKA", "LTU", "LVA", "MLT", "MYS", "ROU", "SAU", "SGP", "THA", "VNM")),
#'             namevar = union(STAN.VARALL, c("FDDE", "FGGE", "FHHE", "GCFI", "INVC", "GDPR")),
#'             isic = 3,
#'             file = file.path(PATH.REPO, "stanData\\data\\STANNAi3.rda"),
#'             replace = TRUE)
#'
#' ## include or update sources, keeping existing sources in file:
#' packageData(list = c("ICIO052013"),
#'             namecou = unique(c(union(STAN.COU, STAN.COUKPC), "BGR", "BRN", "CYP", "HKG", "KHM", "LKA", "LTU", "LVA", "MLT", "MYS", "ROU", "SAU", "SGP", "THA", "VNM")),
#'             namevar = union(STAN.VARALL, c("FDDE", "FGGE", "FHHE", "GCFI", "INVC", "GDPR")),
#'             isic = 3,
#'             file = file.path(PATH.REPO, "stanData\\data\\STANNAi3.rda"),
#'             replace = FALSE)
#'
#' ## create for icioapp:
#' packageData(list = c("ICIOVB2013"),
#'             isic = 3,
#'             nameyear=c(1995:2009),
#'             file = file.path(PATH.REPO, "icioData\\data\\STANICIOi3.rda"),
#'             replace = TRUE)

packageData <- function(list=c("STAN", "BTD"),
                        isic=4,
                        file="data.rda",
                        namecou=character(),
                        namepar=character(),
                        namevar=character(),
                        nameeuc=character(),
                        nameind=character(),
                        nameyear=character(),
                        replace=FALSE,
                        sqlite=FALSE,
                        ...)
{

    if (replace==FALSE & sqlite==FALSE) {
        env <- new.env()
        load(file, envir = env)
        if (!setequal(paste0("DATA.", list), ls(env))) {
            ## rm(list, envir = env)
            ## save(list = list, file = file, envir = env)
            list <- union(list, sub("DATA.", "", ls(env)))
            load(file)
        }
    }
    if (sqlite==TRUE) library(RSQLite)
    require(RODBC)
    require(reshape2)
    if (isic==3 & length(nameind)==0) {
        nameind=STANi3.INDALL
    } else if (isic==4 & length(nameind)==0) {
        nameind=STANi4.INDALL
    }

    if (isic==3) {
        ## SQL sources
        if ("STAN"%in%list) {           # STAN ISIC Rev. 3 in USD
            DATA.STAN <- queryData(connection=SQL.STAN,
                                   namecou=namecou,
                                   table="STANPUB",
                                   isic=isic)[,-6]
            DATA.STAN <- DATA.STAN[!DATA.STAN$var%in%c("EXPO", "IMPO"),]
            ## apply USD exchange rates from SNA Table 4
            load(file.path(PATH.SASi4,"DATA_in", "SNA", "SNA_PPEX.rda"))
            DATA.STAN <- merge(DATA.STAN, DATA.SNAPPEX[DATA.SNAPPEX$var=="EXCH",], by = c("cou", "year"))
            names(DATA.STAN) <-  sub("var.x", "var", names(DATA.STAN))
            names(DATA.STAN) <-  sub("value.x", "value", names(DATA.STAN))
            DATA.STAN$value[DATA.STAN$var%in%STAN.VARMON] <- DATA.STAN$value[DATA.STAN$var%in%STAN.VARMON] / DATA.STAN$value.y[DATA.STAN$var%in%STAN.VARMON]
            DATA.STAN <- subset(DATA.STAN, select = c("cou", "var", "ind", "year", "value"))
            DATA.STAN <- DATA.STAN[DATA.STAN$cou%in%namecou & DATA.STAN$var%in%namevar & DATA.STAN$ind%in%nameind,] # & DATA.STAN$year%in%nameyear,]
        }

        if ("BTD"%in%list) { # STAN BTD ISIC Rev. 3L categ "Total" and partner "WOR" in USD
            techind <- c("HITECH",
                         "MHTECH",
                         "MLTECH",
                         "LOTECH",
                         "HMHTECH",
                         "ICTMAN",
                         "ICTSER",
                         "ENERGYP",
                         "NONMAN")
            nameindBTDi3 <- nameind[!nameind%in%techind]
            nameindBTDi3 <- substr(as.character(nameindBTDi3), 2, nchar(as.character(nameindBTDi3)))
            nameindBTDi3 <- c(nameindBTDi3, intersect(nameind, techind))
            DATA.BTD <- queryData(connection=SQL.STANBTD,
                                  table="BTDIxEi3",
                                  namecou=namecou,
                                  dim.ind="BTD",
                                  nameind=nameindBTDi3,
                                  isic=isic,
                                  add.where=" AND par = 'WOR' AND categ = 'TOTAL'")
            DATA.BTD$ind[!DATA.BTD$BTD%in%techind] <- paste0("C", DATA.BTD$BTD[!DATA.BTD$BTD%in%techind])
            DATA.BTD$ind[DATA.BTD$BTD%in%techind] <- as.character(DATA.BTD$BTD[DATA.BTD$BTD%in%techind])
            DATA.BTD$ind <- as.factor(DATA.BTD$ind)
            DATA.BTD <- subset(DATA.BTD, select=c("cou", "var", "ind", "year", "value"))
            DATA.BTD <- DATA.BTD[DATA.BTD$cou%in%namecou & DATA.BTD$var%in%namevar & DATA.BTD$ind%in%nameind,] # & DATA.BTD$year%in%nameyear,]
        }

        if ("BTDIXE"%in%list) { # BTDIxE ISIC Rev. 3 : PAR and EUC dimesion
            ## tables <- sqlTables(SQL.STANBTD)$TABLE_NAME
            ## h(tables[substr(tables, 1, 3)=="BTD"])
            techind <- c("HITECH",
                         "MHTECH",
                         "MLTECH",
                         "LOTECH",
                         "HMHTECH",
                         "ICTMAN",
                         "ICTSER",
                         "ENERGYP",
                         "NONMAN")
            nameindBTDIXEi3 <- nameind[!nameind%in%techind]
            nameindBTDIXEi3 <- substr(as.character(nameindBTDIXEi3), 2, nchar(as.character(nameindBTDIXEi3)))
            nameindBTDIXEi3 <- c(nameindBTDIXEi3, intersect(nameind, techind))
            DATA.BTDIXE <- queryData(
                connection=SQL.STANBTD,
                table="BTDIXEi3",
                namecou=namecou,
                dim.ind="BTD",
                nameind=nameindBTDIXEi3,
                isic=isic) # , add.where=" AND par = 'WOR'")
            DATA.BTDIXE$ind[!DATA.BTDIXE$BTD%in%techind] <- paste0("C", DATA.BTDIXE$BTD[!DATA.BTDIXE$BTD%in%techind])
            DATA.BTDIXE$ind[DATA.BTDIXE$BTD%in%techind] <- as.character(DATA.BTDIXE$BTD[DATA.BTDIXE$BTD%in%techind])
            DATA.BTDIXE$ind <- as.factor(DATA.BTDIXE$ind)
            names(DATA.BTDIXE) <- sub("CATEG", "euc", names(DATA.BTDIXE))
            DATA.BTDIXE <- subset(DATA.BTDIXE, select=c("cou", "par", "var", "euc", "ind", "year", "value"))
            if (length(namecou)!=0) DATA.BTDIXE <- DATA.BTDIXE[DATA.BTDIXE$cou%in%namecou,]
            if (length(namepar)!=0) DATA.BTDIXE <- DATA.BTDIXE[DATA.BTDIXE$par%in%namepar,]
            if (length(namevar)!=0) DATA.BTDIXE <- DATA.BTDIXE[DATA.BTDIXE$var%in%namevar,]
            if (length(nameeuc)!=0) DATA.BTDIXE <- DATA.BTDIXE[DATA.BTDIXE$euc%in%nameeuc,]
            DATA.BTDIXE <- DATA.BTDIXE[DATA.BTDIXE$ind%in%nameind,] # & DATA.BTDIXE$year%in%nameyear,]
        }

        ## non-SQL sources
        ## source(file.path(PATH.STAN, "1_load_OECDSUT.R"))
        if ("OECDSUT112013"%in%list) { # OECD Annual SUT [DATA.OECDSUT112013]
            ## source("http://oecdshare.oecd.org/sti/eas/stan/STAN_R/1_load_OECDSUT.R")
            load(file.path(PATH.SASi3,"DATA_in", "OECDSUT", "OECDSUT112013.rda"))
            DATA.OECDSUT112013 <- DATA.OECDSUT112013[DATA.OECDSUT112013$cou%in%namecou & DATA.OECDSUT112013$var%in%namevar & DATA.OECDSUT112013$ind%in%nameind,]
        }

        if ("NSONAPATCH"%in%list) { # collected information from NSOs in USD, combined with UNSDSNA for VA
            load(file.path(PATH.COUi3, "NSONAPATCH.rda"))
            DATA.NSONAPATCH <- DATA.NSONAPATCH[DATA.NSONAPATCH$cou%in%namecou & DATA.NSONAPATCH$var%in%namevar & DATA.NSONAPATCH$ind%in%nameind,]
        }

        ## if ("EUANA"%in%list) { # retrieve Eurostat Annual national accounts from SDMX
        ##     load("I:\\STANi4\\SAS07\\SASsystem\\DATA_in\\NAMA\\NAMA.rda")
        ##     h(DATA.NAMAi4)
        ## }

        if ("EUNAIOR1"%in%list) { # converted information from Eurostat SUTs in NACE Rev. 1
            ## source("http://oecdshare.oecd.org/sti/eas/stan/STAN_R/1_load_EUNAIOR1.R")
            load(file.path(PATH.SASi3, "DATA_in", "NAIO", "nace_r1", "EUNAIOR1.rda"))
            DATA.EUNAIOR1 <- DATA.EUNAIOR1[DATA.EUNAIOR1$cou%in%namecou & DATA.EUNAIOR1$var%in%namevar & DATA.EUNAIOR1$ind%in%nameind,]
        }

        if ("WIOD"%in%list) { # WIOD Socio-economic accounts [DATA.WIOD.SEA]
            load(file.path(PATH.SASi3, "DATA_in", "WIOD", "SEA.rda"))
            DATA.WIOD <- DATA.WIOD.SEA[DATA.WIOD.SEA$cou%in%namecou & DATA.WIOD.SEA$var%in%namevar & DATA.WIOD.SEA$ind%in%nameind,]
        }

        if ("UNSDSNA2013"%in%list) {    # UN National Accounts 2013
            load(file.path(PATH.SASi3, "DATA_in", "UN", "UNSD_SNA", "UNSDSNA2013.rda"))
            DATA.UNSDSNA2013 <- DATA.UNSD.SNA[DATA.UNSD.SNA$cou%in%namecou & DATA.UNSD.SNA$var%in%namevar & DATA.UNSD.SNA$ind%in%nameind,]
        }

        if ("UNDATA203100"%in%list) {   # UN Data platform, table 203
            load(file.path(PATH.SASi3, "DATA_in", "UN", "UNSD_MADT", "UNDATA203.rda")) # in USD
            DATA.UNDATA203 <- DATA.UNDATA203[DATA.UNDATA203$cou%in%namecou & DATA.UNDATA203$var%in%namevar & DATA.UNDATA203$ind%in%nameind,]
            DATA.UNDATA203100 <- DATA.UNDATA203[DATA.UNDATA203$series==100,!colnames(DATA.UNDATA203)=="series"]
            DATA.UNDATA203150 <- DATA.UNDATA203[DATA.UNDATA203$series==150,!colnames(DATA.UNDATA203)=="series"]
            DATA.UNDATA203200 <- DATA.UNDATA203[DATA.UNDATA203$series==200,!colnames(DATA.UNDATA203)=="series"]
            DATA.UNDATA203300 <- DATA.UNDATA203[DATA.UNDATA203$series==300,!colnames(DATA.UNDATA203)=="series"]
            DATA.UNDATA203400 <- DATA.UNDATA203[DATA.UNDATA203$series==400,!colnames(DATA.UNDATA203)=="series"]
            DATA.UNDATA203500 <- DATA.UNDATA203[DATA.UNDATA203$series==500,!colnames(DATA.UNDATA203)=="series"]
            list <- c(list, "UNDATA203150", "UNDATA203200", "UNDATA203300", "UNDATA203400", "UNDATA203500")
        }

        if ("UNDATA203CON"%in%list) { # UN Data platform, table 203, connecting all series
            ## source(file.path(PATH.STAN, "1_load_UNSD_UNData.R"))
            load(file.path(PATH.SASi3, "DATA_in", "UN", "UNSD_MADT", "UNDATA203CON.rda")) # in USD
            DATA.UNDATA203CON <- DATA.UNDATA203CON[DATA.UNDATA203CON$cou%in%namecou & DATA.UNDATA203CON$var%in%namevar & DATA.UNDATA203CON$ind%in%nameind,]
            list <- c(list, "UNDATA203CON")
        }
        if ("UNDATAPATCH"%in%list) { # UN Data platform, table 203, connecting all series
            source(file.path(PATH.IO, "2014sut-io", "data-sources", "7_UN", "1_load_UNSD_UNData_SQL.R"))
            writeRDA <- TRUE; writeCSV <- FALSE
            source(file.path(PATH.IO, "2014sut-io", "data-sources", "7_UN", "patch.r"))
            load(file.path(PATH.SASi3, "DATA_in", "UN", "UNSD_MADT", "UNDATAPATCH.rda")) # in USD

            DATA.UNDATAPATCH <- data.patch.usd
            h(DATA.UNDATAPATCH)

            DATA.UNDATAPATCH <- DATA.UNDATAPATCH[DATA.UNDATAPATCH$cou%in%namecou & DATA.UNDATAPATCH$var%in%namevar & DATA.UNDATAPATCH$ind%in%nameind,]
            list <- c(list, "UNDATAPATCH")
        }

        if ("WIOT042012"%in%list) {     # WIOT tables [wiotapr2012]
            load(file.path(PATH.SASi3, "DATA_in", "WIOD", "WIOTapr2012.rda"))
            DATA.WIOT042012 <- wiotapr2012
            names(DATA.WIOT042012) <- c("cou", "var", "code", "year", "value")
            DATA.WIOT042012$code <- sub("Total", "TOT", DATA.WIOT042012$code)
            ## STAN industry codes
            convind <- read.csv(file.path(PATH.SASi3, "DATA_in", "WIOD", "WIOD_ind.csv"))
            DATA.WIOT042012 <- merge(DATA.WIOT042012, convind, by.x = "code", by.y = "WIOD")
            DATA.WIOT042012 <- DATA.WIOT042012[,-1]
            ## STAN industry aggregates
            DATA.WIOT042012 <- DATA.WIOT042012[!DATA.WIOT042012$ind=="CTOTAL",] # issues with CTOTAL: two values per year
            DATA.WIOT042012 <- DATA.WIOT042012[!DATA.WIOT042012$var=="TBAL",] # empty values
            data <- dcast(DATA.WIOT042012, cou + var + year ~ ind, value.var="value")
            data.agg <- indAggregate(data=data, isic=3)
            DATA.WIOT042012 <- melt(data.agg, id.vars=c("cou", "var", "year"), variable.name="ind")
            DATA.WIOT042012 <- DATA.WIOT042012[DATA.WIOT042012$cou%in%namecou & DATA.WIOT042012$var%in%namevar & DATA.WIOT042012$ind%in%nameind,] # & DATA.WIOT042012$year%in%nameyear,]
        }

        if ("WIOT112013"%in%list) {     # WIOT tables [wiotnov2013]
            load(file.path(PATH.SASi3, "DATA_in", "WIOD", "WIOTnov2013.rda"))
            DATA.WIOT112013 <- wiod_nov2013
            names(DATA.WIOT112013) <- c("cou", "var", "code", "year", "value")
            DATA.WIOT112013$code <- sub("Total", "TOT", DATA.WIOT112013$code)
            ## STAN industry codes
            convind <- read.csv(file.path(PATH.SASi3, "DATA_in", "WIOD", "WIOD_ind.csv"))
            DATA.WIOT112013 <- merge(DATA.WIOT112013, convind, by.x = "code", by.y = "WIOD")
            DATA.WIOT112013 <- DATA.WIOT112013[,-1]
            ## STAN industry aggregates
            ## CTOTAL only in VALU, PROD
            ## unique(DATA.WIOT112013$ind[DATA.WIOT112013$var=="VALU"])
            ## unique(DATA.WIOT112013$ind[DATA.WIOT112013$var=="EXPO"])
            ## data <- DATA.WIOT112013[DATA.WIOT112013$var%in%c("VALU", "PROD"),]
            data <- DATA.WIOT112013
            data <- data[!data$ind=="CTOTAL",] # issues with CTOTAL : two values per year
            data <- data[!data$var=="TBAL",] # empty values
            data.d <- dcast(data, cou + var + year ~ ind, value.var="value")
            data.agg <- indAggregate(data=data.d, isic=3)
            data.agg.m <- melt(data.agg, id.vars=c("cou", "var", "year"), variable.name="ind")
            ## DATA.WIOT112013 <- rbind(data.agg.m, wiod_nov2013[wiod_nov2013$var%in%c("EXPO", "IMPO"),])
            DATA.WIOT112013 <- data.agg.m
            DATA.WIOT112013 <- DATA.WIOT112013[DATA.WIOT112013$cou%in%namecou & DATA.WIOT112013$var%in%namevar & DATA.WIOT112013$ind%in%nameind,] # & DATA.WIOT112013$year%in%nameyear,]
        }

        if ("ICIO052013"%in%list) {     # ICIO May 2013
            load(file.path(PATH.SASi3, "DATA_in", "ICIO1305", "ICIOmay2013 18 ind.rdata"))
            DATA.ICIO052013 <- temp
            load(file.path(PATH.SASi3, "DATA_in", "ICIO1305", "ICIOmay2013 37 ind.rdata"))
            DATA.ICIO052013 <- rbind(DATA.ICIO052013, temp)
            ## DATA.ICIO052013 <- matrix(temp[64:length(temp)], ncol = 5, byrow = TRUE)
            ## DATA.ICIO052013 <- data.frame(DATA.ICIO052013)
            names(DATA.ICIO052013) <- c("cou", "var", "ind", "year", "value")
            DATA.ICIO052013 <- DATA.ICIO052013[!duplicated(DATA.ICIO052013),]
            DATA.ICIO052013$ind <- paste0("C", DATA.ICIO052013$ind)
            ## DATA.ICIO052013 <- DATA.ICIO052013[!DATA.ICIO052013$cou==1,]
            ## DATA.ICIO052013$ind <- sub("C75T95", "C75T99", DATA.ICIO052013$ind)
            ## DATA.ICIO052013$year <- as.integer(as.character(DATA.ICIO052013$year))
            ## DATA.ICIO052013$value <- as.numeric(as.character(DATA.ICIO052013$value))
            DATA.ICIO052013.d <- dcast(DATA.ICIO052013, cou + var + year ~ ind, value.var="value")
            DATA.ICIO052013.d <- indAggregate(data = DATA.ICIO052013.d, isic = 3)
            DATA.ICIO052013 <- melt(DATA.ICIO052013.d, id.vars=c("cou", "var", "year"), variable.name="ind")
            DATA.ICIO052013 <- DATA.ICIO052013[DATA.ICIO052013$cou%in%namecou & DATA.ICIO052013$var%in%namevar & DATA.ICIO052013$ind%in%nameind,] # & DATA.ICIO052013$year%in%nameyear,]
        }

        if ("ICIO6137VB"%in%list) { # ICIO 61 countries, 37 industries
            ## dir.create(file.path(tempdir(), "results"))
            ## takes about one hour
            source(file.path(PATH.REPO, "icioapp61", "1212io-indic61_add_gfcf_hhcp.R"))
            ## temppath <- "D:\\icio_vB_23_01_2013"
            icioyear <- c(1995, 2000, 2005, 2008, 2009)
            years <- intersect(nameyear, icioyear)
            ncou <- 61
            nind <- 37
            DATA.ICIO6137VB <- array(0, dim=c(ncou * nind, ncou * nind, length(years)))
            DATA.ICIO6137EB <- array(0, dim=c(ncou * nind, ncou * nind, length(years)))
            DATA.ICIO6137FDTTLWITHDISC <- array(0, dim=c(ncou * nind, ncou, length(years)))
            DATA.ICIO6137HHCP <- array(0, dim=c(ncou * nind, ncou, length(years)))
            DATA.ICIO6137GFCF <- array(0, dim=c(ncou * nind, ncou, length(years)))
            DATA.ICIO6137GRTR <- array(0, dim=c(ncou * nind, ncou, length(years)))
            for (yr in seq(along=years))
            {
                load(file.path(tempdir(), "results_", years[yr], ".rda"))
                ## load(file.path(temppath, "\\results_", years[yr], ".rda"))
                DATA.ICIO6137VB[,,yr] <- vB
                DATA.ICIO6137EB[,,yr] <- eB
                DATA.ICIO6137FDTTLWITHDISC[,,yr] <- fd_ttl_withDISC
                DATA.ICIO6137HHCP[,,yr] <- hhcp
                DATA.ICIO6137GFCF[,,yr] <- gfcf
                DATA.ICIO6137GRTR[,,yr] <- grtr61
            }
            list <- c(list, "ICIO6137EB", "ICIO6137FDTTLWITHDISC", "ICIO6137HHCP", "ICIO6137GFCF", "ICIO6137GRTR")
        }

        if ("ICIO5837VB"%in%list) { # ICIO 58 countries, 37 industries
            source(file.path(PATH.REPO, "icioapp", "requested_indic_024tim-wiod.R"))
            icioyear <- c(1995, 2000, 2005, 2008, 2009)
            years <- intersect(nameyear, icioyear)
            ncou <- 58
            nind <- 37
            DATA.ICIO5837VB <- array(0, dim=c(ncou * nind, ncou * nind, length(years)))
            DATA.ICIO5837EB <- array(0, dim=c(ncou * nind, ncou * nind, length(years)))
            DATA.ICIO5837FDTTLWITHDISC <- array(0, dim=c(ncou * nind, ncou, length(years)))
            DATA.ICIO5837HHCP <- array(0, dim=c(ncou * nind, ncou, length(years)))
            DATA.ICIO5837GFCF <- array(0, dim=c(ncou * nind, ncou, length(years)))
            DATA.ICIO5837GRTR <- array(0, dim=c(ncou * nind, ncou, length(years)))
            for (yr in seq(along=years))
            {
                load(file.path(tempdir(), "results_", years[yr], ".rda"))
                DATA.ICIO5837VB[,,yr] <- vB
                DATA.ICIO5837EB[,,yr] <- eB
                DATA.ICIO5837FDTTLWITHDISC[,,yr] <- fd_ttl_withDISC
                DATA.ICIO5837HHCP[,,yr] <- hhcp
                DATA.ICIO5837GFCF[,,yr] <- gfcf
                DATA.ICIO5837GRTR[,,yr] <- grtr
            }
            list <- c(list, "ICIO5837EB", "ICIO5837FDTTLWITHDISC", "ICIO5837HHCP", "ICIO5837GFCF", "ICIO5837GRTR")
        }

        if ("INDSTAT32"%in%list) { # UNIDO Indstat ISIC Rev. 3 2-digit
            load(file.path(PATH.SASi3, "DATA_in", "UNIDO", "INDSTAT32.rda"))
            DATA.INDSTAT32.d <- dcast(DATA.INDSTAT32, cou + var + year ~ ind, value.var="value")
            DATA.INDSTAT32.d <- indAggregate(data = DATA.INDSTAT32.d, isic = 3)
            DATA.INDSTAT32 <- melt(DATA.INDSTAT32.d, id.vars=c("cou", "var", "year"), variable.name="ind")
            DATA.INDSTAT32 <- DATA.INDSTAT32[DATA.INDSTAT32$cou%in%namecou & DATA.INDSTAT32$var%in%namevar & DATA.INDSTAT32$ind%in%nameind,] # & DATA.INDSTAT32$year%in%nameyear,]
        }

    } else if (isic==4)
    {
        ## SQL sources
        if ("STAN"%in%list) {           # STAN ISIC Rev. 4 in USD
            DATA.STAN <- queryData(connection=SQL.STAN,
                                    table="STANPUBi4_PRE",
                                    isic=isic)[,-6]
            DATA.STAN <- DATA.STAN[!DATA.STAN$var%in%c("EXPO", "IMPO"),]
            DATA.STAN <- DATA.STAN[DATA.STAN$cou%in%namecou & DATA.STAN$var%in%namevar & DATA.STAN$ind%in%nameind,] # & DATA.STAN$year%in%nameyear,]
        }

        if ("BTD"%in%list) {            # BTDIxE ISIC Rev. 4 in USD
            DATA.BTD <- queryData(connection=SQL.STANBTD,
                                   table="BTDIxEi4",
                                   dim.ind="BTD",
                                   isic=isic,
                                   add.where=" AND par = 'WOR' AND categ = 'TOTAL'")
            names(DATA.BTD) <- sub("BTD", "ind", names(DATA.BTD))
            DATA.BTD <- subset(DATA.BTD, select=c("cou", "var", "ind", "year", "value"))
            DATA.BTD <- DATA.BTD[DATA.BTD$cou%in%namecou & DATA.BTD$var%in%namevar & DATA.BTD$ind%in%nameind,] # & DATA.BTD$year%in%nameyear,]
        }

        if ("ANBERD"%in%list) {         # ANBERD ISIC Rev. 4 in USD
            DATA.ANBERD <- sqlQuery(SQL.STAN, "SELECT * FROM ANBERD_REV4_PUB")
            ## DATA.ANBERD <- sqlQuery(SQL.STAN, "SELECT * FROM ANBERD_WORK_REV4 WHERE NOT sou = 'QUEST_PF'")
            X <- strsplit(as.character(DATA.ANBERD$cou), "_")
            DATA.ANBERD$cou <- sapply(X, "[[", 1)
            DATA.ANBERD$type <- sapply(X, "[[", 2)
            DATA.ANBERD$var[DATA.ANBERD$type=="MA"] <- "RDNC"
            DATA.ANBERD <- DATA.ANBERD[DATA.ANBERD$cou%in%namecou & DATA.ANBERD$var%in%namevar & DATA.ANBERD$ind%in%nameind,] # & DATA.ANBERD$year%in%nameyear,]
        }

        if ("XRATES"%in%list) {         # Exchange rates and PPPs
            DATA.XRATES <- sqlQuery(SQL.STAN, "SELECT * FROM XRATESMII")
        }

        if ("LFSEU"%in%list) {          # European labour force survey
            ## source(file.path(PATH.SKILL, "data", "_EULFS", "extract_EULFS.R"))
            load(file.path(PATH.SKILL, "data", "_EULFS", "OECD_130603_2.Rda"))
            DATA.LFSEU <- DATA.LFSEU[DATA.LFSEU$cou%in%namecou & DATA.LFSEU$var%in%namevar & DATA.LFSEU$ind%in%nameind,] # & DATA.LFSEU$year%in%nameyear,]
        }

        if("LFSAUS"%in%list) {          # US labour force survey
            ## source(file.path(PATH.SKILL, "data", "AUS", "read_LFS_AUS.R"))
            load(file.path(PATH.SKILL, "data", "AUS", "LFS_AUS_February.Rda"))
            DATA.LFSAUS$cou <- as.factor("AUS")
            DATA.LFSAUS$var <- as.factor("EMPN")
            DATA.LFSAUS <- subset(DATA.LFSAUS, select = c("cou", "var", "ind", "ocu", "year", "value"))
            DATA.LFSAUS <- DATA.LFSAUS[DATA.LFSAUS$cou%in%namecou & DATA.LFSAUS$var%in%namevar & DATA.LFSAUS$ind%in%nameind,] # & DATA.LFSAUS$year%in%nameyear,]
        }

        if("LFSCAN"%in%list)            # US labour force survey
        {
            ## source(file.path(PATH.SKILL, "data", "CAN", "read_LFS_CAN.R"))
            load(file.path(PATH.SKILL, "data", "CAN", "LFS_CAN_noc2011.Rda"))
            DATA.LFSCAN$cou <- as.factor("CAN")
            ## DATA.LFSCAN$var <- "EMPN"
            DATA.LFSCAN <- subset(DATA.LFSCAN, select = c("cou", "var", "ind", "ocu", "year", "value"))
            DATA.LFSCAN <- DATA.LFSCAN[DATA.LFSCAN$cou%in%namecou & DATA.LFSCAN$var%in%namevar & DATA.LFSCAN$ind%in%nameind,] # & DATA.LFSCAN$year%in%nameyear,]
        }

        if("LFSUSA"%in%list) {          # US labour force survey
            ## source(file.path(PATH.SKILL, "data", "USA", "read_LFS_USA.R")) # change nameyear
            load(file.path(PATH.SKILL, "data", "USA", "LFS_USA_March_isco2008.Rda"))
            DATA.LFSUSA$cou <- as.factor("USA")
            ## DATA.LFSUSA$var <- "EMPN"
            DATA.LFSUSA <- subset(DATA.LFSUSA, select = c("cou", "var", "ind", "ocu", "year", "value"))
            ## unique(DATA.LFSUSA$year)
            DATA.LFSUSA <- DATA.LFSUSA[DATA.LFSUSA$cou%in%namecou & DATA.LFSUSA$var%in%namevar & DATA.LFSUSA$ind%in%nameind,] # & DATA.LFSUSA$year%in%nameyear,]
        }

        if("LFSILO"%in%list) {   # ILOSTAT data ISIC Rev. 4, ISCO 2008
            ## source(file.path(PATH.SKILL, "data", "_ILO", "extract_ILOSTAT.R"))
            load(file.path(PATH.SKILL, "data", "_ILO", "EMP_ECO_OCU_ISIC4_ISCO2008.Rda"))
            DATA.LFSILO <- DATA.LFSILO[DATA.LFSILO$cou%in%namecou & DATA.LFSILO$var%in%namevar & DATA.LFSILO$ind%in%nameind,] # & DATA.LFSILO$year%in%nameyear,]
        }

    }
    if (sqlite==FALSE) {
        save(list = paste0("DATA.", list), file = file)
        addDatalist(file = file, list = paste0("DATA.", list))
    } else {
        ## file = file.path(PATH.REPO, "btdData", "data", "BTDIXEi3.db")
        ## list = "BTDIXE"
        diskdb <- dbConnect(SQLite(), dbname = file)
        ok <- dbWriteTable(conn = diskdb, name = list, value = eval(parse(text=paste0("DATA.", list))), row.names = FALSE, overwrite = TRUE)
    }
}

## additional ISIC Rev. 4 sources

## ## Eurostat Annual National Accounts (NAMA) [DATA.NAMAi4]
## load(file.path(PATH.SASi4,"DATA_in", "NAMA", "NAMA.rda"))

## ## STD Annual National Accounts (SNA) [DATA.SNAi4]
## load(file.path(PATH.SASi4,"DATA_in", "SNA", "SNA.rda"))
