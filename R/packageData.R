#' Package Data
#'
#' Package National Accounts data sources
#'
#' This function prepares R data files for packaging. It is primarily intended to maintain data sources from National Accounts (in million USD). For example, \code{sou="BTD"} and \code{isic=3} will generate the data set \code{DATA.BTDi3} in the specified \code{rdata} file.
#'
#' @param file the exported \code{rdata} file.
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
        list.exist <- ls(env)
        ## do not reload existing items that are in list
        list.exist <- list.exist[!list.exist%in%paste0('DATA.', list)]
        ## remove contents of list from memory
        load(file)
        rm(list=paste0('DATA.', list))
        ## if (!setequal(paste0("DATA.", list), ls(env))) {
        ##     list <- union(list, sub("DATA.", "", ls(env))) # modify: currently re-loads
        ##     load(file)
        ## }
    }
    if (sqlite==TRUE) library(RSQLite)
    require(RJSDMX)
    require(RODBC)
    require(reshape2)
    require(dplyr)
    require(stanData); data(STANNAi0) # USD exchange rates - see isic==0

    if (isic==3 & length(nameind)==0) {
        nameind=STANi3.INDALL
    } else if (isic==4 & length(nameind)==0) {
        nameind=STANi4.INDALL
    }

    if (isic==0) {

        if ("CONVCUR"%in%list) {


            ## ##################################### ##
            ##  from UNSD Main Aggregates (implied)  ##
            ## ##################################### ##

            ## loadfile <- file.path(PATH.SASi3, "DATA_in", "UN", "UNSD_SNA", "DATA.UNSDEXCH.rda")
            ## use function stanData/R/transUNSDSNA.R
            DATA.UNSDEXCH <- transUNSDSNA(xrates=TRUE, download=TRUE, year.min=1970)
            save(DATA.UNSDEXCH, file = loadfile)

            ## subset(STAN.COUEN, cou=="CZE")
            ##
            ## [DATA.UNSDEXCH$cou=='CZE',]
            ## test <- subset(DATA.UNSDEXCH, cou=="CZE")
            ## test[order(test$year),]

            load(file = loadfile)

            DATA.UNSDEXCH <- subset(DATA.UNSDEXCH, cou%in%namecou & var%in%namevar)
            ## DATA.XRATES <- subset(DATA.XRATES, select = c("cou", "var", "ind", "year", "value"))

            ## backward compatibility
            DATA.XRATES <- DATA.UNSDEXCH

            ## ## not used:
            ## DATA.CONVCUR <- list()
            ## DATA.CONVCUR$UNSDMA <- DATA.UNSDEXCH

            ##
            list <- c(list, "XRATES")
            ## save(list = c("DATA.CONVCUR", "DATA.XRATES"), file = file.path(PATH.REPO, "stanData", "data", "STANNAi0.rda"))
            ## list <- c(list, "XRATES")
            ## ############## ##
            ##  from IMF IFS  ##
            ## ############## ##
            ## IMF: this provider was removed (same as ISTAT)
            ## Yes, it was SOAP based and I'm redeveloping it for using REST. Unfortunately ISTAT has only the SOAP interface but I want to discontinue it...

            ## ############### ##
            ##  from OECD SNA  ##
            ## ############### ##
            ## see "XRATES" in ISIC Rev. 4 section below and previous method at end of file
            ## load(file.path(PATH.SASi4,"DATA_in", "SNA", "SNA_PPEX.rda"))
            ## DATA.XRATES <- DATA.SNAPPEX

            ## ## ############## ##
            ## ##  from ECB EXR  ##
            ## ## ############## ##
            ## provider <- "ECB"
            ## ## getFlows(provider)
            ## flow <- "EXR"
            ## query.freq <- "A"
            ## query.exr_type <- "SP00"
            ## query.exr_suffix <- "A"
            ## ## #############################
            ## ## get NAC to EUR exchange rates
            ## ## #############################
            ## TScodes.cur <- names(getCodes(provider, flow, "CURRENCY"))
            ## conv.cur <- merge(subset(STAN.COUCUR, cou%in%namecou), data.frame(cur = TScodes.cur))
            ## query.currency <- gsub(", ", "+", toString(union(unique(conv.cur$cur), "USD")))
            ## query.currency_denom <- "EUR"

            ## NAC2EUR<- getSDMX(provider, paste0(paste(flow,
            ##                                          query.freq,
            ##                                          query.currency,
            ##                                          query.currency_denom,
            ##                                          query.exr_type,
            ##                                          query.exr_suffix,
            ##                                          sep = '.')), start = "1970")
            ## ##
            ## NAC2EUR <- sdmxTS2DF(SDMXTS = NAC2EUR, provider = provider, timevar = "year", numeric = TRUE)
            ## NAC2EUR <- merge(NAC2EUR, subset(NAC2EUR, CURRENCY=="USD"), by = names(NAC2EUR)[!names(NAC2EUR)%in%c("CURRENCY", "value")])
            ## NAC2EUR$value <- NAC2EUR$value.x / NAC2EUR$value.y
            ## NAC2EUR <- NAC2EUR[!is.na(NAC2EUR$value),]
            ## NAC2EUR <- merge(NAC2EUR, subset(STAN.COUCUR, cou%in%namecou), by.x = "CURRENCY.x", by.y = "cur")
            ## NAC2EUR$var <- "EXCH"
            ## NAC2EUR <- subset(NAC2EUR, select = c("var", "cou", "year", "value"))
            ## NAC2EUR <- NAC2EUR[order(NAC2EUR$cou, NAC2EUR$year),]

            ## ##
            ## names(SDMXTS) <- sub("LOCATION", "cou", names(SDMXTS))
            ## names(SDMXTS) <- sub("TRANSACT", "var", names(SDMXTS))
            ## SDMXTS$var[SDMXTS$var=="EXC"] <- "EXCH"
            ## SDMXTS$var[SDMXTS$var=="PPPGDP"] <- "PPPS"
            ## SDMXTS <- SDMXTS[!is.na(SDMXTS$value),]
            ## ##
            ## DATA.XRATES <- subset(SDMXTS, select = c("var", "cou", "year", "value"))
            ## DATA.XRATES <- subset(DATA.XRATES, cou%in%namecou & var%in%namevar)

            ## ####################################### ##
            ## TODO:                                   ##
            ## combine ECB and OECD SNA exchange rates ##
            ## ####################################### ##

        }

    }

    if (isic==3) {

        if ("BTDi3"%in%list) { # STAN BTD ISIC Rev. 3L categ "Total" and partner "WOR" in USD
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
            DATA.BTDi3 <- queryData(connection=SQL.STANBTD,
                                  table="BTDIxEi3",
                                  namecou=namecou,
                                  dim.ind="BTD",
                                  nameind=nameindBTDi3,
                                  isic=isic,
                                  add.where=" AND par = 'WOR' AND categ = 'TOTAL'")
            DATA.BTDi3$ind[!DATA.BTDi3$BTD%in%techind] <- paste0("C", DATA.BTDi3$BTD[!DATA.BTDi3$BTD%in%techind])
            DATA.BTDi3$ind[DATA.BTDi3$BTD%in%techind] <- as.character(DATA.BTDi3$BTD[DATA.BTDi3$BTD%in%techind])
            DATA.BTDi3$ind <- as.factor(DATA.BTDi3$ind)
            DATA.BTDi3 <- subset(DATA.BTDi3, select=c("cou", "var", "ind", "year", "value"))
            DATA.BTDi3 <- subset(DATA.BTDi3, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if ("BTDIXEi3"%in%list) { # BTDIxE ISIC Rev. 3 : PAR and EUC dimesion
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
            DATA.BTDIXEi3 <- queryData(
                connection=SQL.STANBTD,
                table="BTDIXEi3",
                namecou=namecou,
                dim.ind="BTD",
                nameind=nameindBTDIXEi3,
                isic=isic) # , add.where=" AND par = 'WOR'")
            DATA.BTDIXEi3$ind[!DATA.BTDIXEi3$BTD%in%techind] <- paste0("C", DATA.BTDIXEi3$BTD[!DATA.BTDIXEi3$BTD%in%techind])
            DATA.BTDIXEi3$ind[DATA.BTDIXEi3$BTD%in%techind] <- as.character(DATA.BTDIXEi3$BTD[DATA.BTDIXEi3$BTD%in%techind])
            DATA.BTDIXEi3$ind <- as.factor(DATA.BTDIXEi3$ind)
            names(DATA.BTDIXEi3) <- sub("CATEG", "euc", names(DATA.BTDIXEi3))
            DATA.BTDIXEi3 <- subset(DATA.BTDIXEi3, select=c("cou", "par", "var", "euc", "ind", "year", "value"))
            if (length(namecou)!=0) DATA.BTDIXEi3 <- DATA.BTDIXEi3[DATA.BTDIXEi3$cou%in%namecou,]
            if (length(namepar)!=0) DATA.BTDIXEi3 <- DATA.BTDIXEi3[DATA.BTDIXEi3$par%in%namepar,]
            if (length(namevar)!=0) DATA.BTDIXEi3 <- DATA.BTDIXEi3[DATA.BTDIXEi3$var%in%namevar,]
            if (length(nameeuc)!=0) DATA.BTDIXEi3 <- DATA.BTDIXEi3[DATA.BTDIXEi3$euc%in%nameeuc,]
            DATA.BTDIXEi3 <- subset(DATA.BTDIXEi3, ind%in%nameind)
        }

        if ("EUNAIOR1"%in%list) { # converted information from Eurostat SUTs in NACE Rev. 1
            ## source("http://oecdshare.oecd.org/sti/eas/stan/STAN_R/1_load_EUNAIOR1.R")
            load(file.path(PATH.SASi3, "DATA_in", "NAIO", "nace_r1", "EUNAIOR1.rda"))
            DATA.EUNAIOR1 <- subset(DATA.EUNAIOR1, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if ("EUNAMAR1"%in%list) {
            ## require(stanData)
            ## transEUNAMA(isic = 3, download = FALSE, year.min = 1970)
            load(file.path(PATH.SASi3, "DATA_in", "NAMA", "EUNAMAR1.rda"))
            ## unique(DATA.EUNAMAR1$cou)
            ## apply USD exchange rates
            DATA.EUNAMAR1 <- merge(DATA.EUNAMAR1, DATA.XRATES[DATA.XRATES$var=="EXCH",], by = c("cou", "year"))
            names(DATA.EUNAMAR1) <-  sub("var.x", "var", names(DATA.EUNAMAR1))
            names(DATA.EUNAMAR1) <-  sub("value.x", "value", names(DATA.EUNAMAR1))
            DATA.EUNAMAR1$value[DATA.EUNAMAR1$var%in%STAN.VAR[["MON"]]] <- DATA.EUNAMAR1$value[DATA.EUNAMAR1$var%in%STAN.VAR[["MON"]]] / DATA.EUNAMAR1$value.y[DATA.EUNAMAR1$var%in%STAN.VAR[["MON"]]]
            DATA.EUNAMAR1 <- subset(DATA.EUNAMAR1, select = c("cou", "var", "ind", "year", "value"))
            DATA.EUNAMAR1 <- subset(DATA.EUNAMAR1, cou%in%namecou & var%in%namevar & ind%in%nameind)
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
            DATA.ICIO052013 <- subset(DATA.ICIO052013, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if ("ICIO5837VB"%in%list) { # ICIO 58 countries, 37 industries
            source(file.path(dbpath, "GitHub", "icioData", "data-raw", "requested_indic_024tim-wiod.R"))
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

        if ("ICIO6234VB"%in%list) { # ICIO 62 countries, 34 industries
            source(file.path(dbpath, "GitHub", "icioData", "data-raw", "transICIO6234APP.R"))
            ## icioyear <- c(1995:2011)
            ## length(icioyear)
            ## years <- intersect(nameyear, icioyear)
            DATA.ICIO6234VB <- vB
            DATA.ICIO6234EB <- DATA.ICIO6234VB  # replace later

            ## DATA.ICIO6234GRTR <- grtr[c(1:(dim(grtr)[1]-1)), c(1:dim(grtr)[2]-1),]  # remove "Total" from dimension 1 and 2
            DATA.ICIO6234GRTR <- grtr[, , c(1:dim(grtr)[3]-1)]  # remove "DISC" from dimension 3
            DATA.ICIO6234FDTTLWITHDISC <- DATA.ICIO6234GRTR  # replace later
            DATA.ICIO6234HHCP <- DATA.ICIO6234GRTR  # replace later
            DATA.ICIO6234GFCF <- DATA.ICIO6234GRTR  # replace later

            list <- c(list, "ICIO6234EB", "ICIO6234FDTTLWITHDISC", "ICIO6234HHCP", "ICIO6234GFCF", "ICIO6234GRTR")
            ## list <- c(list, "ICIO6234GRTR")
        }

        if ("ICIO6137VB"%in%list) { # ICIO 61 countries, 37 industries
            ## dir.create(file.path(tempdir(), "results"))
            ## takes about one hour
            source(file.path(dbpath, "GitHub", "icioapp61", "1212io-indic61_add_gfcf_hhcp.R"))
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

        if ("INDSTAT32"%in%list) { # UNIDO Indstat ISIC Rev. 3 2-digit
            ## require(stanData)
            ## transINDSTAT(isic = 3, detail = 2, path = file.path(PATH.SASi3, "DATA_in", "UNIDO", "INDSTAT_2_2012"), year.min = 1970)
            load(file.path(PATH.SASi3, "DATA_in", "UNIDO", "INDSTAT32.rda"))
            ## DATA.INDSTAT32.d <- dcast(DATA.INDSTAT32, cou + var + year ~ ind, value.var="value")
            ## DATA.INDSTAT32.d <- indAggregate(data = DATA.INDSTAT32.d, isic = 3)
            ## DATA.INDSTAT32 <- melt(DATA.INDSTAT32.d, id.vars=c("cou", "var", "year"), variable.name="ind")
            DATA.INDSTAT32 <- subset(DATA.INDSTAT32, year >= 1970)
            DATA.INDSTAT32 <- subset(DATA.INDSTAT32, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if ("NSONAPATCHi3"%in%list) { # collected information from NSOs in USD, combined with UNSDSNA for VA
            ## see section "combine all swn.csv files in NSONAPATCH" of file J:\STAN07\COU\master.R
            load(file.path(PATH.SASi3, "DATA_out", "NSONAPATCHi3.rda"))
            DATA.NSONAPATCHi3 <- subset(DATA.NSONAPATCHi3, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        ## non-SQL sources
        ## source(file.path(PATH.STAN, "1_load_OECDSUT.R"))
        if ("OECDSUT112013"%in%list) { # OECD Annual SUT [DATA.OECDSUT112013]
            ## source("http://oecdshare.oecd.org/sti/eas/stan/STAN_R/1_load_OECDSUT.R")
            load(file.path(PATH.SASi3,"DATA_in", "OECDSUT", "OECDSUT112013.rda"))
            DATA.OECDSUT112013 <- subset(DATA.OECDSUT112013, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if ("STANi3"%in%list) {           # STAN ISIC Rev. 3 in USD
            DATA.STANi3 <- queryData(connection=SQL.STAN,
                                   namecou=namecou,
                                   table="STANPUB",
                                   isic=isic)[,-6]
            DATA.STANi3 <- DATA.STANi3[!DATA.STANi3$var%in%c("EXPO", "IMPO"),]
            ## apply USD exchange rates from SNA Table 4
            load(file.path(PATH.SASi4,"DATA_in", "SNA", "SNA_PPEX.rda"))
            DATA.STANi3 <- merge(DATA.STANi3, DATA.SNAPPEX[DATA.SNAPPEX$var=="EXCH",], by = c("cou", "year"))
            names(DATA.STANi3) <-  sub("var.x", "var", names(DATA.STANi3))
            names(DATA.STANi3) <-  sub("value.x", "value", names(DATA.STANi3))
            DATA.STANi3$value[DATA.STANi3$var%in%STAN.VARMON] <- DATA.STANi3$value[DATA.STANi3$var%in%STAN.VARMON] / DATA.STANi3$value.y[DATA.STANi3$var%in%STAN.VARMON]
            DATA.STANi3 <- subset(DATA.STANi3, select = c("cou", "var", "ind", "year", "value"))
            DATA.STANi3 <- subset(DATA.STANi3, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if ("STDSNAi3"%in%list) {
            ## require(stanData)
            ## transSTDSNA(channel = SQL.SNA, isic = 4, year.min = 1970)
            load(file.path(PATH.SASi3, "DATA_in", "SNA", "STDSNAi3.rda"))
            DATA.STDSNAi3 <- convertCurrency(data=DATA.STDSNAi3, datacur=DATA.XRATES[DATA.XRATES$var=="EXCH",])
            DATA.STDSNAi3 <- subset(DATA.STDSNAi3, select = c("cou", "var", "ind", "year", "value"))
            DATA.STDSNAi3 <- subset(DATA.STDSNAi3, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if ("UNDATA203100"%in%list) {   # UN Data platform, table 203
            load(file.path(PATH.SASi3, "DATA_in", "UN", "UNSD_MADT", "UNDATA203.rda")) # in USD
            DATA.UNDATA203 <- subset(DATA.UNDATA203, cou%in%namecou & var%in%namevar & ind%in%nameind)
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
            DATA.UNDATA203CON <- subset(DATA.UNDATA203CON, cou%in%namecou & var%in%namevar & ind%in%nameind)
            list <- c(list, "UNDATA203CON")
        }

        if ("UNSDSNA2013"%in%list) {    # UN National Accounts 2013
            load(file.path(PATH.SASi3, "DATA_in", "UN", "UNSD_SNA", "UNSDSNA2013.rda"))
            DATA.UNSDSNA2013 <- subset(DATA.UNSD.SNA, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if ("WIOD"%in%list) { # WIOD Socio-economic accounts [DATA.WIOD.SEA]
            load(file.path(PATH.SASi3, "DATA_in", "WIOD", "SEA.rda"))
            DATA.WIOD <- subset(DATA.WIOD.SEA, cou%in%namecou & var%in%namevar & ind%in%nameind)
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
            DATA.WIOT042012 <- subset(DATA.WIOT042012, cou%in%namecou & var%in%namevar & ind%in%nameind)
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
            DATA.WIOT112013 <- subset(DATA.WIOT112013, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

    } else if (isic==4)
    {

        ## if ("XRATES"%in%list) {

        ##     ## using RJSDMX
        ##     ## namecou <- namecou[1:3]
        ##     ## http://stats.oecd.org/Index.aspx?DataSetCode=SNA_TABLE4
        ##     provider <- "OECD"
        ##     getFlows(provider)
        ##     flow <- "SNA_TABLE4"
        ##     ##
        ##     TScodes.cou <- names(getCodes(provider, flow, "LOCATION"))
        ##     namecou <- namecou[namecou%in%TScodes.cou]
        ##     query.cou <- gsub(", ", "+", toString(namecou))
        ##     ##
        ##     query.var <- gsub(", ", "+", toString(namevar[namevar%in%c("EXCH", "PPPS")]))
        ##     query.var <- sub("EXCH", "EXC", query.var)
        ##     query.var <- sub("PPPS", "PPPGDP", query.var)
        ##     ##
        ##     SDMXTS <- getSDMX(provider, paste0(paste(flow, query.cou, query.var, 'CD.A', sep = '.')), start = "1970")
        ##     ##
        ##     SDMXTS <- sdmxTS2DF(
        ##         SDMXTS
        ##         ,
        ##         provider
        ##         ,
        ##         timevar = "year"
        ##         ,
        ##         numeric = TRUE
        ##         )
        ##     ##
        ##     names(SDMXTS) <- sub("LOCATION", "cou", names(SDMXTS))
        ##     names(SDMXTS) <- sub("TRANSACT", "var", names(SDMXTS))
        ##     SDMXTS$var[SDMXTS$var=="EXC"] <- "EXCH"
        ##     SDMXTS$var[SDMXTS$var=="PPPGDP"] <- "PPPS"
        ##     SDMXTS <- SDMXTS[!is.na(SDMXTS$value),]
        ##     ##
        ##     DATA.XRATES <- subset(SDMXTS, select = c("var", "cou", "year", "value"))
        ##     DATA.XRATES <- subset(DATA.XRATES, cou%in%namecou & var%in%namevar)

        ## }

        if ("ANBERDi4"%in%list) {         # ANBERD ISIC Rev. 4 in USD
            DATA.ANBERDi4 <- sqlQuery(SQL.STAN, "SELECT * FROM ANBERD_REV4_PUB")
            ## DATA.ANBERDi4 <- sqlQuery(SQL.STAN, "SELECT * FROM ANBERD_WORK_REV4 WHERE NOT sou = 'QUEST_PF'")
            X <- strsplit(as.character(DATA.ANBERDi4$cou), "_")
            DATA.ANBERDi4$cou <- sapply(X, "[[", 1)
            DATA.ANBERDi4$type <- sapply(X, "[[", 2)
            DATA.ANBERDi4 <- rbind(DATA.ANBERDi4[DATA.ANBERDi4$type=="MA",],
                                 DATA.ANBERDi4[DATA.ANBERDi4$type!="MA",])
            DATA.ANBERDi4 <- DATA.ANBERDi4[!duplicated(DATA.ANBERDi4[,colnames(DATA.ANBERDi4)%in%c("cou", "ind", "year")]),]
            ## DATA.ANBERDi4$var[DATA.ANBERDi4$type=="MA"] <- "RDNC"
            DATA.ANBERDi4$var <- "RDNC"
            ## DATA.ANBERDi4 <- DATA.ANBERDi4[, colnames(DATA.ANBERDi4)!="type"]
            DATA.ANBERDi4 <- subset(DATA.ANBERDi4, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if ("BTDi4"%in%list) {            # BTDIxE ISIC Rev. 4 in USD
            DATA.BTDi4 <- queryData(connection=SQL.STANBTD,
                                   table="BTDIxEi4",
                                   dim.ind="BTD",
                                   isic=isic,
                                   add.where=" AND par = 'WOR' AND categ = 'TOTAL'")
            names(DATA.BTDi4) <- sub("BTD", "ind", names(DATA.BTDi4))
            DATA.BTDi4 <- subset(DATA.BTDi4, select=c("cou", "var", "ind", "year", "value"))
            DATA.BTDi4 <- subset(DATA.BTDi4, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if ("EUNAMAR2"%in%list) {
            ## source(dbpath, "GitHub", "stanData", "data-raw", "transEUNAMA.R")
            ## transEUNAMA(isic = 4, download = FALSE, year.min = 1970)
            load(file.path(PATH.SASi4, "DATA_in", "NAMA", "EUNAMAR2.rda"))
            ## DATA.XRATES[DATA.XRATES$cou=='CZE',]
            ## STAN.COUEN[STAN.COUEN$cou=='CZE',]
            ## nrow(subset(DATA.EUNAMAR2, year > 2011))
            ## nrow(subset(DATA.XRATES, year > 2010))
            DATA.EUNAMAR2 <- DATA.EUNAMAR2[!(DATA.EUNAMAR2$cou=="IRL" & DATA.EUNAMAR2$var=="PROD" & DATA.EUNAMAR2$ind=="DTOTAL"),] # See email CW Mon 20-Oct-2014 6:45 PM
            ## h(DATA.XRATES)
            DATA.EUNAMAR2 <- merge(DATA.EUNAMAR2, DATA.XRATES[DATA.XRATES$var=="EXCH",], by = c("cou", "year"))
            names(DATA.EUNAMAR2) <-  sub("var.x", "var", names(DATA.EUNAMAR2))
            names(DATA.EUNAMAR2) <-  sub("value.x", "value", names(DATA.EUNAMAR2))
            DATA.EUNAMAR2$value[DATA.EUNAMAR2$var%in%STAN.VAR[["MON"]]] <- DATA.EUNAMAR2$value[DATA.EUNAMAR2$var%in%STAN.VAR[["MON"]]] / DATA.EUNAMAR2$value.y[DATA.EUNAMAR2$var%in%STAN.VAR[["MON"]]]
            DATA.EUNAMAR2 <- subset(DATA.EUNAMAR2, select = c("cou", "var", "ind", "year", "value"))
            DATA.EUNAMAR2 <- subset(DATA.EUNAMAR2, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if ("EUNAMA10R2"%in%list) {
            ## ## source(file.path(dbpath, "GitHub", "stanData", "data-raw", "transEUNAMA.R"))
            ## DATA.EUNAMA10R2 <- transEUNAMA10(download = FALSE, year.min = 1970)
            ## save(DATA.EUNAMA10R2, file = file.path(path, "EUNAMA10R2.rda"))
            ##
            load(file.path(PATH.SASi4, "DATA_in", "NAMA10", "EUNAMA10R2.rda"))
            ## DATA.XRATES[DATA.XRATES$cou=='CZE',]
            ## STAN.COUEN[STAN.COUEN$cou=='CZE',]
            ## nrow(subset(DATA.EUNAMAR2, year > 2011))
            ## nrow(subset(DATA.XRATES, year > 2010))
            ## DATA.EUNAMAR2 <- DATA.EUNAMAR2[!(DATA.EUNAMAR2$cou=="IRL" & DATA.EUNAMAR2$var=="PROD" & DATA.EUNAMAR2$ind=="DTOTAL"),] # See email CW Mon 20-Oct-2014 6:45 PM
            ## h(DATA.XRATES)
            ## DATA.EUNAMAR2 <- merge(DATA.EUNAMAR2, DATA.XRATES[DATA.XRATES$var=="EXCH",], by = c("cou", "year"))
            ## names(DATA.EUNAMAR2) <-  sub("var.x", "var", names(DATA.EUNAMAR2))
            ## names(DATA.EUNAMAR2) <-  sub("value.x", "value", names(DATA.EUNAMAR2))
            ## DATA.EUNAMAR2$value[DATA.EUNAMAR2$var%in%STAN.VAR[["MON"]]] <- DATA.EUNAMAR2$value[DATA.EUNAMAR2$var%in%STAN.VAR[["MON"]]] / DATA.EUNAMAR2$value.y[DATA.EUNAMAR2$var%in%STAN.VAR[["MON"]]]
            ## DATA.EUNAMAR2 <- subset(DATA.EUNAMAR2, select = c("cou", "var", "ind", "year", "value"))

            ## data already contains 2014 but exchange rates end in 2013

            ## h(subset(DATA.EUNAMA10R2, year==2014))
            ## h(subset(DATA.XRATES, year==2014))
            ## h(subset(data, year==2012))
            ## h(DATA.XRATES)

            DATA.EUNAMA10R2 <- convertCurrency(data = DATA.EUNAMA10R2, tounit = "USD")
            DATA.EUNAMA10R2 <- subset(DATA.EUNAMA10R2, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if ("LFSEU"%in%list) {          # European labour force survey
            ## source(file.path(PATH.SKILL, "data", "_EULFS", "extract_EULFS.R"))
            load(file.path(PATH.SKILL, "data", "_EULFS", "OECD_130603_2.Rda"))
            DATA.LFSEU <- subset(DATA.LFSEU, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if("LFSAUS"%in%list) {          # US labour force survey
            ## source(file.path(PATH.SKILL, "data", "AUS", "read_LFS_AUS.R"))
            load(file.path(PATH.SKILL, "data", "AUS", "LFS_AUS_February.Rda"))
            DATA.LFSAUS$cou <- as.factor("AUS")
            DATA.LFSAUS$var <- as.factor("EMPN")
            DATA.LFSAUS <- subset(DATA.LFSAUS, select = c("cou", "var", "ind", "ocu", "year", "value"))
            DATA.LFSAUS <- subset(DATA.LFSAUS, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if("LFSCAN"%in%list)            # US labour force survey
        {
            ## source(file.path(PATH.SKILL, "data", "CAN", "read_LFS_CAN.R"))
            load(file.path(PATH.SKILL, "data", "CAN", "LFS_CAN_noc2011.Rda"))
            DATA.LFSCAN$cou <- as.factor("CAN")
            ## DATA.LFSCAN$var <- "EMPN"
            DATA.LFSCAN <- subset(DATA.LFSCAN, select = c("cou", "var", "ind", "ocu", "year", "value"))
            DATA.LFSCAN <- subset(DATA.LFSCAN, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if("LFSUSA"%in%list) {          # US labour force survey
            ## source(file.path(PATH.SKILL, "data", "USA", "read_LFS_USA.R")) # change nameyear
            load(file.path(PATH.SKILL, "data", "USA", "LFS_USA_March_isco2008.Rda"))
            DATA.LFSUSA$cou <- as.factor("USA")
            ## DATA.LFSUSA$var <- "EMPN"
            DATA.LFSUSA <- subset(DATA.LFSUSA, select = c("cou", "var", "ind", "ocu", "year", "value"))
            ## unique(DATA.LFSUSA$year)
            DATA.LFSUSA <- subset(DATA.LFSUSA, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if("LFSILO"%in%list) {   # ILOSTAT data ISIC Rev. 4, ISCO 2008
            ## source(file.path(PATH.SKILL, "data", "_ILO", "extract_ILOSTAT.R"))
            load(file.path(PATH.SKILL, "data", "_ILO", "EMP_ECO_OCU_ISIC4_ISCO2008.Rda"))
            DATA.LFSILO <- DATA.LFSILO[DATA.LFSILO$cou%in%namecou & DATA.LFSILO$var%in%namevar & DATA.LFSILO$ind%in%nameind,] # & DATA.LFSILO$year%in%nameyear,]
        }

        if ("NSONAPATCHi4"%in%list) { # collected information from NSOs in USD, combined with UNSDSNA for VA
            ## see section "combine all swn.csv files in NSONAPATCH" of file J:\STAN07\COU\master.R
            load(file.path(PATH.SASi4, "DATA_out", "NSONAPATCHi4.rda"))
            DATA.NSONAPATCHi4 <- subset(DATA.NSONAPATCHi4, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if("PIAAC"%in%list) {           # PIAAC data ASAP4
            ## source(file.path(PATH.SKILL, "data", "_PIAAC", "extract_PIAAC.R"))
            load(file.path(PATH.SKILL, "data", "_PIAAC", "CNTRY_ISCO08_ISIC4.Rda"))
            DATA.PIAAC <- subset(DATA.PIAAC, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if ("STANi4"%in%list) {           # STAN ISIC Rev. 4 in USD

            ## ## SQL.STAN <- odbcDriverConnect(connection = "SERVER=VS-GEN-SQL-3; DRIVER=SQL Server; DATABASE=STAN", readOnlyOptimize = TRUE)
            ## ## isic <- 4
            ## DATA.STANi4 <- queryData(connection=SQL.STAN,
            ##                         table="STANPUBi4_PRE",
            ##                         isic=isic)[,-6]
            ## DATA.STANi4 <- DATA.STANi4[!DATA.STANi4$var%in%c("EXPO", "IMPO"),]
            ## data <- DATA.STANi4
            ## df.d <- dcast(DATA.STANi4, cou + ind + year ~ var, value.var = "value")
            ## df.d.pyp <- df.d %>%
            ##     group_by(ind, cou) %>%
            ##         mutate(VKPY = lag(VALU) * VALK / lag(VALK),
            ##                PKPY = lag(PROD) * PRDK / lag(PRDK),
            ##                IKPY = lag(INTI) * INTK / lag(INTK),
            ##                GKPY = lag(GFCF) * GFCK / lag(GFCK),
            ##                CNPY = lag(CAPN) * CPNK / lag(CPNK),
            ##                CGPY = lag(CAPG) * CPGK / lag(CPGK))
            ## df.m <- melt(df.d.pyp, id.vars = c("cou", "ind", "year"), variable.name = "var", na.rm=TRUE)
            ## DATA.STANi4 <- df.m
            ## ## DATA.STANi4 <- subset(DATA.STANi4, !is.na(value))
            ## save(DATA.STANi4, file = file.path(PATH.SASi4, "DATA_in", "STAN_SQL", "STANi4_NAC.rda"))

            ## ## ## convert to USD
            ## ## require(stanData)
            ## ## data(STANNAi0)
            load(file.path(PATH.SASi4, "DATA_in", "STAN_SQL", "STANi4_NAC.rda"))
            DATA.STANi4 <- convertCurrency(data=DATA.STANi4, datacur=DATA.XRATES[DATA.XRATES$var=="EXCH",])
            save(DATA.STANi4, file = file.path(PATH.SASi4, "DATA_in", "STAN_SQL", "STANi4_USD.rda"))
            ## load(file.path(PATH.SASi4, "DATA_in", "STAN_SQL", "STANi4_USD.rda"))
            ## write.csv(DATA.STANi4, file = file.path(dbpath, "Public", "stani4.csv"))
            DATA.STANi4 <- subset(DATA.STANi4, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if ("STDSSISi4"%in%list) {

            namedim <- dotStatGetDimensionList(channel = SQL.STAT,
                                               datasetcode = "SSIS_BSC_ISIC4",
                                               lang = "en")
            dim.list <- lapply(namedim, dotStatGetDimensionMemberList,
                               channel = SQL.STAT,
                               datasetcode = "SSIS_BSC_ISIC4",
                               lang = "en")
            names(dim.list) <- namedim
            namelocation.stan <- intersect(dim.list[["LOCATION"]], STAN.COU[["ICIO"]])
            namevar.stan <- intersect(dim.list[["VAR"]], STAN.VAR[["ALL"]])
            nameind.ssis <- STANi4.IND[["ALL"]]
            nameind.ssis <- sub("D", "", nameind.ssis)
            nameind.ssis <- sub("T", "_", nameind.ssis)
            nameind.ssis <- nameind.ssis[nameind.ssis%in%dim.list[["ISIC4"]]]

            DATA.STDSSISi4 <- transSTDSSIS(channel = SQL.STAT,
                                           isic = 4,
                                           year.min = 1970,
                                           namelocation = namelocation.stan,
                                           namevar = namevar.stan,
                                           nameisic = nameind.ssis)

            save(DATA.STDSSISi4, file = file.path(PATH.SASi4, "DATA_in", "SSIS", "STDSSISi4_NAC.rda"))


            ### ## convert to USD
            ## require(stanData)
            ## data(STANNAi0)
            load(file.path(PATH.SASi4, "DATA_in", "SSIS", "STDSSISi4_NAC.rda"))
            DATA.STDSSISi4 <- convertCurrency(data=DATA.STDSSISi4, datacur=DATA.XRATES[DATA.XRATES$var=="EXCH",])
            save(DATA.STDSSISi4, file = file.path(PATH.SASi4, "DATA_in", "SNA", "STDSSISi4_USD.rda"))

            DATA.STDSSISi4 <- subset(DATA.STDSSISi4, select = c("cou", "var", "ind", "year", "value"))
            ## load(file.path(PATH.SASi4, "DATA_in", "SNA", "STDSSISi4_USD.rda"))

            DATA.STDSSISi4 <- subset(DATA.STDSSISi4, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if ("STDSNAi4"%in%list) {

            ## ## source(file.path(dbpath, "GitHub", "stanData", "R", "transSTDSNA.R"))
            ## ## "SQL.SNA" defined in Rinitfunctions.r
            ## DATA.STDSNAi4 <- transSTDSNA(channel = SQL.SNA, isic = 4, year.min = 1970)
            ## ## ## compare with previous data
            ## ## DATA.STDSNAi4.new <- DATA.STDSNAi4
            ## ## load(file.path(PATH.SASi4, "DATA_in", "SNA", "STDSNAi4.rda"))
            ## ## str(DATA.STDSNAi4); str(DATA.STDSNAi4.new)
            ## ## setdiff(unique(DATA.STDSNAi4.new$cou), unique(DATA.STDSNAi4$cou))
            ## ## setdiff(unique(subset(DATA.STDSNAi4.new, cou=="USA")$var), unique(subset(DATA.STDSNAi4, cou=="USA")$var))
            ## ## DATA.STDSNAi4 <- DATA.STDSNAi4.new
            ## ## add pyp variables
            ## df.d <- reshape2::dcast(DATA.STDSNAi4, cou + ind + year ~ var, value.var = "value")
            ## ## ## ## debug
            ## ## ## devtools::install(file.path(dbpath, "GitHub", "stan"))
            ## ## ## data <- df.d
            ## ## ## var.cp <- "VALU"
            ## ## ## var.cl <- "VALK"
            ## ## id.vars.SNA <- c("cou", "ind")
            ## ## refyear.SNA <- 2010
            ## ## df.d$VKPY <- stan::cpVolPyp(data=df.d,
            ## ##                             var.cp="VALU",
            ## ##                             var.cl="VALK",
            ## ##                             id.vars=id.vars.SNA)
            ## ## df.d$GKPY <- stan::cpVolPyp(data=df.d,
            ## ##                             var.cp="GFCF",
            ## ##                             var.cl="GFCK",
            ## ##                             id.vars=id.vars.SNA)
            ## ## df.m <- melt(df.d, id.vars = c("cou", "ind", "year"), variable.name = "var", na.rm=TRUE)
            ## df.d.pyp <- df.d %>%
            ##     group_by(ind, cou) %>%
            ##         mutate(VKPY = lag(VALU) * VALK / lag(VALK),
            ##                GKPY = lag(GFCF) * GFCK / lag(GFCK))
            ## df.m <- melt(df.d.pyp, id.vars = c("cou", "ind", "year"), variable.name = "var", na.rm=TRUE)
            ## DATA.STDSNAi4 <- df.m
            ## DATA.STDSNAi4 <- subset(DATA.STDSNAi4, !is.na(value))
            ## max(DATA.STDSNAi4$year[DATA.STDSNAi4$cou=="USA"])
            ## save(DATA.STDSNAi4, file = file.path(PATH.SASi4, "DATA_in", "SNA", "STDSNAi4_NAC.rda"))

            ## convert to USD
            load(file.path(PATH.SASi4, "DATA_in", "SNA", "STDSNAi4_NAC.rda"))
            DATA.STDSNAi4 <- convertCurrency(data=DATA.STDSNAi4, datacur=DATA.XRATES[DATA.XRATES$var=="EXCH",])
            DATA.STDSNAi4 <- subset(DATA.STDSNAi4, select = c("cou", "var", "ind", "year", "value"))
            ## save(DATA.STDSNAi4, file = file.path(PATH.SASi4, "DATA_in", "SNA", "STDSNAi4_USD.rda"))
            ## load(file.path(PATH.SASi4, "DATA_in", "SNA", "STDSNAi4_USD.rda"))

            DATA.STDSNAi4 <- subset(DATA.STDSNAi4, cou%in%namecou & var%in%namevar & ind%in%nameind)
        }

        if ("UNDATA206SNA93"%in%list) { # UN Data platform, table 203, connecting all series
            ## source(file.path(dbpath, "GitHub", "stanData", "data-raw", "transUNDATA.R"))
            ## save(DATA.UNDATA206SNA93, file = file.path(PATH.SASi4, "DATA_in", "UN", "UNSD_MADT", "UNDATA206SNA93.rda"))
            load(file.path(PATH.SASi4, "DATA_in", "UN", "UNSD_MADT", "UNDATA206SNA93.rda")) # in USD
            DATA.UNDATA206SNA93 <- subset(DATA.UNDATA206SNA93, cou%in%namecou & var%in%namevar & ind%in%nameind)
            list <- c(list, "UNDATA206SNA93")
        }

        if ("UNDATA206SNA08"%in%list) { # UN Data platform, table 203, connecting all series
            ## source(file.path(PATH.STAN, "1_load_UNSD_UNData.R"))
            ## save(DATA.UNDATA206SNA08, file = file.path(PATH.SASi4, "DATA_in", "UN", "UNSD_MADT", "UNDATA206SNA08.rda"))
            load(file.path(PATH.SASi4, "DATA_in", "UN", "UNSD_MADT", "UNDATA206SNA08.rda")) # in USD
            DATA.UNDATA206SNA08 <- subset(DATA.UNDATA206SNA08, cou%in%namecou & var%in%namevar & ind%in%nameind)
            list <- c(list, "UNDATA206SNA08")
        }

    }
    if (sqlite==FALSE) {
        if (replace==FALSE) {
            list <- c(sub("DATA.", "", list.exist), list)
        }
        ## ## delete object from Rda
        ## env <- new.env()
        ## data(STANNAi4, envir = env)
        ## list <- ls(env)
        ## list <- list[list!="DATA.XRATES"]
        ## save(list = list, file = file, envir = env)
        ## ##
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

## Exchange rates and PPPs - old version
## DATA.XRATES <- sqlQuery(SQL.STAN, "SELECT * FROM XRATESMII")
## url.append <- paste0("/all")
## code.all <- sdmxRead(api="http://stats.oecd.org/SDMX-JSON",
##                      scheme="codelist",
##                      DSD="SNA_TABLE4",
##                      filter=filter.list,
##                      append=url.append,
##                      query=FALSE)
## ##
## filter.list <- lapply(code.all, '[[', 1)
## filter.list <- filter.list[!names(filter.list)=="TIME_PERIOD"]
## conv.var <- rbind.data.frame(c("EXC", "EXCH"),
##                              c("PPPGDP", "PPPS"))
## ##
## names(conv.var) <- c("transact", "var")
## url.append <- paste0('/all?', paste('json-lang=en', 'detail=Full', 'dimensionAtObservation=AllDimensions', sep = '&'))
## ##
## data.all <- NULL
## ## for (var in conv.var$transact) {
## for (var in conv.var$transact[1]) { # PPPGDP doesn't work
##     filter.list[["TRANSACT"]] <- var
##     data <- sdmxRead(api="http://stats.oecd.org/SDMX-JSON",
##                      scheme="data",
##                      DSD="SNA_TABLE4",
##                      filter=filter.list,
##                      append=url.append,
##                      query=FALSE)
##     data.all <- rbind(data.all, data)
## }
## if (any("EXCH"%in%namevar, "PPPS"%in%namevar)) {
