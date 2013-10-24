#' Package Data
#'
#' Package National Accounts data sources
#'
#' This function prepares R data files for packaging. It is primarily intended to maintain data sources from National Accounts (in million USD).
#'
#' @param list a character vector of National Accounts data source IDs.
#' @param isic an integer specifying the ISIC classification of data sources.
#' @param namecou a character vector of 3-digit ISO country codes read from column \sQuote{cou}.
#' @param namevar a character vector of variables read from column \sQuote{var}.
#' @param nameind a character vector of industries read from column \sQuote{ind} or from \sQuote{dim.ind} if specified.
#' @param nameyear and integer vector specifying the selected period.
#' @param replace specify if the existing data shall be replaced.
#'
#' @author OECD STAN
#' @keywords package
#' @seealso \code{\link{queryData}}, \code{\link{addDatalist}}
#' @export
#' @examples
#' packageData(list = c("WIOT042012"), isic = 3)
#' ## Package multiple sources
#' packageData(list = c("STAN", "WIOT042012"), isic = 3, file = paste0(PATH.REPO, "stanData\\data\\STANNAi3.rda"))
#' packageData(list = c("STAN", "BTD"), isic = 4, file = paste0(PATH.REPO, "stanData\\data\\STANNAi4.rda"))

packageData <- function(list=c("STAN", "BTD"),
                        isic=4,
                        file="data.rda",
                        namecou=STAN.COU,
                        namevar=STAN.VARALL,
                        nameind=character(),
                        nameyear=character(),
                        replace=FALSE,
                        ...)
{

    if (replace==FALSE)
    {
        env <- new.env()
        load(file, envir = env)
        if (!setequal(paste0("DATA.", list), ls(env)))
        {
            ## rm(list, envir = env)
            ## save(list = list, file = file, envir = env)
            list <- union(list, sub("DATA.", "", ls(env)))
            load(file)
        }
    }
    require(RODBC)
    require(reshape2)
    if (isic==3 & length(nameind)==0)
    {
        nameind=STANi3.INDALL
    } else if (isic==4 & length(nameind)==0)
    {
        nameind=STANi4.INDALL
    }

    if (isic==3)
    {
        ## SQL sources
        if ("STAN"%in%list)             # STAN ISIC Rev. 3
        {
            DATA.STAN <- queryData(connection=SQL.STAN,
                                    table="STANPUB",
                                    isic=isic)[,-6]
            ## apply USD exchange rates
            load(paste0(PATH.SASi4,'DATA_in\\SNA\\SNA_PPEX.rda'))
            DATA.STAN <- merge(DATA.STAN, DATA.SNAPPEX[DATA.SNAPPEX$var=="EXCH",], by = c("cou", "year"))
            names(DATA.STAN) <-  sub("var.x", "var", names(DATA.STAN))
            names(DATA.STAN) <-  sub("value.x", "value", names(DATA.STAN))
            DATA.STAN$value[DATA.STAN$var%in%STAN.VARMON] <- DATA.STAN$value[DATA.STAN$var%in%STAN.VARMON] / DATA.STAN$value.y[DATA.STAN$var%in%STAN.VARMON]
            DATA.STAN <- subset(DATA.STAN, select = c("cou", "var", "ind", "year", "value"))
        }

        if ("BTD"%in%list)              # BTDIxE ISIC Rev. 3
        {
            techind <- c('HITECH',
                         'MHTECH',
                         'MLTECH',
                         'LOTECH',
                         'HMHTECH',
                         'ICTMAN',
                         'ICTSER',
                         'ENERGYP',
                         'NONMAN')
            nameindBTDi3 <- nameind[!nameind%in%techind]
            nameindBTDi3 <- substr(as.character(nameindBTDi3), 2, nchar(as.character(nameindBTDi3)))
            nameindBTDi3 <- c(nameindBTDi3, intersect(nameind, techind))
            DATA.BTD <- queryData(connection=SQL.STANBTD,
                                   table="BTDIxEi3",
                                   dim.ind="BTD",
                                   nameind=nameindBTDi3,
                                   isic=isic,
                                   add.where=" AND par = 'WOR' AND categ = 'TOTAL'")
            DATA.BTD$ind[!DATA.BTD$BTD%in%techind] <-paste0("C", DATA.BTD$BTD[!DATA.BTD$BTD%in%techind])
            DATA.BTD$ind[DATA.BTD$BTD%in%techind] <- as.character(DATA.BTD$BTD[DATA.BTD$BTD%in%techind])
            DATA.BTD$ind <- as.factor(DATA.BTD$ind)
            DATA.BTD <- subset(DATA.BTD, select=c("cou", "var", "ind", "year", "value"))
        }

        ## non-SQL sources
        if ("OECDSUT102013"%in%list) # OECD Annual SUT [DATA.OECDSUT102013]
        {
            load(paste0(PATH.SASi3,'DATA_in\\OECDSUT\\OECDSUT102013.rda'))
            DATA.OECDSUT102013 <- DATA.OECDSUT102013[DATA.OECDSUT102013$cou%in%namecou & DATA.OECDSUT102013$var%in%namevar & DATA.OECDSUT102013$ind%in%nameind,]
        }

        if ("WIOD"%in%list) # WIOD Socio-economic accounts [DATA.WIOD.SEA]
        {
            load(paste0(PATH.SASi3,'DATA_in\\WIOD\\SEA.rda'))
            DATA.WIOD <- DATA.WIOD.SEA[DATA.WIOD.SEA$cou%in%namecou & DATA.WIOD.SEA$var%in%namevar & DATA.WIOD.SEA$ind%in%nameind,]
        }

        if ("UNSDSNA2013"%in%list)      # UN National Accounts 2013
        {
            load(paste0(PATH.SASi3,'DATA_in\\UN\\UNSD_SNA\\UNSDSNA2013.rda'))
            DATA.UNSDSNA2013 <- DATA.UNSD.SNA[DATA.UNSD.SNA$cou%in%namecou & DATA.UNSD.SNA$var%in%namevar & DATA.UNSD.SNA$ind%in%nameind,]
        }

        if ("WIOT042012"%in%list)       # WIOT tables [wiotapr2012]
        {
            load(paste0(PATH.SASi3, "DATA_in\\WIOD\\WIOTapr2012.rda"))
            DATA.WIOT042012 <- wiotapr2012
            names(DATA.WIOT042012) <- c("cou", "var", "code", "year", "value")
            DATA.WIOT042012$code <- sub("Total", "TOT", DATA.WIOT042012$code)
            ## STAN industry codes
            convind <- read.csv(paste0(PATH.SASi3, "DATA_in\\WIOD\\WIOD_ind.csv"))
            DATA.WIOT042012 <- merge(DATA.WIOT042012, convind, by.x = 'code', by.y = 'WIOD')
            DATA.WIOT042012 <- DATA.WIOT042012[,-1]
            ## STAN industry aggregates
            DATA.WIOT042012 <- DATA.WIOT042012[!DATA.WIOT042012$ind=="CTOTAL",] # issues with CTOTAL: two values per year
            DATA.WIOT042012 <- DATA.WIOT042012[!DATA.WIOT042012$var=="TBAL",] # empty values
            data <- dcast(DATA.WIOT042012, cou + var + year ~ ind, value.var="value")
            data.agg <- indAggregate(data=data, isic=3)
            DATA.WIOT042012 <- melt(data.agg, id.vars=c("cou", "var", "year"), variable.name="ind")
            ## ind <- "CTOTAL"
            ## var <- "VALU"
            ## namecou <- unique(DATA.WIOT042012$cou)
            ## nameyear <- unique(DATA.WIOT042012$year)
            ## data <- DATA.WIOT042012[DATA.WIOT042012$ind==ind & DATA.WIOT042012$var==var,]
            ## data.all <- NULL
            ## for (cou in namecou)
            ## {
            ##     data.cou <- data[data$cou==cou,]
            ##     for (year in nameyear)
            ##     {
            ##         data.year <- data.cou[data.cou$year==year,]
            ##         data.year$var[data.year$value==max(data.year$value)] <- "PROD"
            ##         data.all <- rbind(data.all, data.year)
            ##     }
            ## }
            ## DATA.WIOT042012 <- DATA.WIOT042012[!DATA.WIOT042012$ind=="CTOTAL",] # issues with CTOTAL: two values per year
            ## DATA.WIOT042012 <- DATA.WIOT042012[!DATA.WIOT042012$var=="TBAL",] # empty values
            ## DATA.WIOT042012 <- rbind(DATA.WIOT042012, data.all)
            ## data <- dcast(DATA.WIOT042012, cou + var + year ~ ind, value.var="value")
            DATA.WIOT042012 <- DATA.WIOT042012[DATA.WIOT042012$cou%in%namecou & DATA.WIOT042012$var%in%namevar & DATA.WIOT042012$ind%in%nameind,] # & DATA.WIOT042012$year%in%nameyear,]
        }

        if ("ICIO052013"%in%list)             # ICIO May 2013
        {
            load(paste0(PATH.SASi3, "DATA_in\\ICIO1305\\ICIOmay2013 18 ind.rdata"))
            DATA.ICIO052013 <- temp
            load(paste0(PATH.SASi3, "DATA_in\\ICIO1305\\ICIOmay2013 37 ind.rdata"))
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

        if ("INDSTAT32"%in%list)             # UNIDO Indstat ISIC Rev. 3 2-digit
        {
            load(paste0(PATH.SASi3, "DATA_in\\UNIDO\\INDSTAT32.rda"))
            DATA.INDSTAT32.d <- dcast(DATA.INDSTAT32, cou + var + year ~ ind, value.var="value")
            DATA.INDSTAT32.d <- indAggregate(data = DATA.INDSTAT32.d, isic = 3)
            DATA.INDSTAT32 <- melt(DATA.INDSTAT32.d, id.vars=c("cou", "var", "year"), variable.name="ind")
            DATA.INDSTAT32 <- DATA.INDSTAT32[DATA.INDSTAT32$cou%in%namecou & DATA.INDSTAT32$var%in%namevar & DATA.INDSTAT32$ind%in%nameind,] # & DATA.INDSTAT32$year%in%nameyear,]
        }

    } else if (isic==4)
    {
        ## SQL sources
        if ("STAN"%in%list)             # STAN ISIC Rev. 4
        {
            DATA.STAN <- queryData(connection=SQL.STAN,
                                    table="STANPUBi4_PRE",
                                    isic=isic)[,-6]
        }
        if (isic==4 & "BTD"%in%list)    # BTDIxE ISIC Rev. 4
        {
            DATA.BTD <- queryData(connection=SQL.STANBTD,
                                   table="BTDIxEi4",
                                   dim.ind="BTD",
                                   isic=isic,
                                   add.where=" AND par = 'WOR' AND categ = 'TOTAL'")
            names(DATA.BTD) <- sub("BTD", "ind", names(DATA.BTD))
            DATA.BTD <- subset(DATA.BTD, select=c("cou", "var", "ind", "year", "value"))
        }
    }
    save(list = paste0("DATA.", list), file = file)
    addDatalist(file = file, list = paste0("DATA.", list))

}

## additional ISIC Rev. 4 sources

## ## Eurostat Annual National Accounts (NAMA) [DATA.NAMAi4]
## load(paste0(PATH.SASi4,'DATA_in\\NAMA\\NAMA.rda'))

## ## STD Annual National Accounts (SNA) [DATA.SNAi4]
## load(paste0(PATH.SASi4,'DATA_in\\SNA\\SNA.rda'))
