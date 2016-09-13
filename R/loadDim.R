#' Load Dimensions
#'
#' Load dimension members
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
                    file=file.path(PATH.REPO, "stan", "data", "stanDim.rda")
                   ,
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
    ## require(XLConnect)
    ## STAN.COU <- readWorksheetFromFile(file = file.path(PATH.SASi4, "lists/STAN_cou_list.xls"), sheet = 1)
    ## names(STAN.COU) <- tolower(names(STAN.COU))



    ## STAN.COU <- read.csv(file = file.path(dbpath, "GitHub", "stan", "inst", "loadDim_cou.csv"))
    ## STAN.COU <- read.csv(file = file.path(find.package("stan"), "loadDim_cou.csv"))
    STAN.COU <- read.csv(system.file("extdata", "loadDim_cou.csv", package = "stan"))
    id <- "reg."
    namereg <- names(STAN.COU)[substr(names(STAN.COU), 1, nchar(id))==id]
    list.all <- NULL
    ## reg <- namereg[1]
    for (reg in namereg) {
      list.reg <- list(as.character(STAN.COU$iso3[STAN.COU[[reg]]==1]))
      names(list.reg) <- substr(reg, 5, nchar(reg))
      list.all <- c(list.all, list.reg)
    }
    STAN.COU <- list.all
    ## STAN.COUEU <- STAN.COU[["EU"]]
    ## STAN.COUKPC <- STAN.COU[["KPC"]]

    ## to match with UNSD main aggregates
    ## STAN.COUEN <- read.csv(file = file.path(find.package("stan"), "loadDim_countryUNen.csv"))
    STAN.COUEN <- read.csv(system.file("extdata", "loadDim_countryUNen.csv", package = "stan"))

    STAN.COUEN$cou <- as.factor(STAN.COUEN$cou)

    ## conversion lists
    ## cou2eunama <- unique(data.m$geo)
    ## couv.cou.init <- merge(data.frame(cou2 = cou2eunama), STAN.COU2, all.x = TRUE)
    ## cat(paste0('c("', couv.cou.init$cou2, '", "', couv.cou.init$cou, '"),\n'))

    STAN.COUEUROSTAT <- rbind.data.frame(c("AUT", "AT"),
                                         c("BEL", "BE"),
                                         c("BGR", "BG"),
                                         c("CHE", "CH"),
                                         c("CYP", "CY"),
                                         c("CZE", "CZ"),
                                         c("DEU", "DE"),
                                         c("DNK", "DK"),
                                         c("EST", "EE"),
                                         c("GRC", "EL"),
                                         c("ESP", "ES"),
                                         c("FIN", "FI"),
                                         c("FRA", "FR"),
                                         c("HRV", "HR"),
                                         c("HUN", "HU"),
                                         c("IRL", "IE"),
                                         c("ISL", "IS"),
                                         c("ITA", "IT"),
                                         c("JPN", "JP"),
                                         c("LIE", "LI"),
                                         c("LTU", "LT"),
                                         c("LUX", "LU"),
                                         c("LVA", "LV"),
                                         c("MNE", "ME"),
                                         c("MKD", "MK"),
                                         c("MLT", "MT"),
                                         c("NLD", "NL"),
                                         c("NOR", "NO"),
                                         c("POL", "PL"),
                                         c("PRT", "PT"),
                                         c("ROU", "RO"),
                                         c("SRB", "RS"),
                                         c("SWE", "SE"),
                                         c("SVN", "SI"),
                                         c("SVK", "SK"),
                                         c("TUR", "TR"),
                                         c("GBR", "UK"),
                                         c("USA", "US")
                                         ## c("NA", "EU15"),
                                         ## c("NA", "EU27"),
                                         ## c("NA", "EA"),
                                         ## c("NA", "EA12"),
                                         ## c("NA", "EA17")
                                         )
    names(STAN.COUEUROSTAT) <- c("cou","eurostat")

    list <- c(list, "STAN.COU", "STAN.COUEN", "STAN.COUEUROSTAT") # STAN.COU: STAN.COU[["OECD"]]
  }

  if ("var"%in%dim)
  {
    ## require(XLConnect)
    ## detach("package:XLConnect", unload=TRUE)
    ## STAN.VAR <- readWorksheetFromFile(file = file.path(PATH.SASi4, "lists/STAN_var_list.xls"), sheet = 1)
    ## names(STAN.VAR) <- tolower(names(STAN.VAR))
    ## STAN.VARLABEL  <- STAN.VAR[,colnames(STAN.VAR)%in%c("var","lbvar_en")]
    ## names(STAN.VARLABEL) <- c("var","label")
    ## STAN.VARLABEL$label <- properCase(as.character(STAN.VARLABEL$label))

    ## STAN.VARALL <- as.factor(STAN.VAR$var)
    ## STAN.VARCLP <- as.factor(STAN.VAR[!is.na(STAN.VAR$clp) & STAN.VAR$clp==1,2])
    ## STAN.VARPYP <- as.factor(STAN.VAR[!is.na(STAN.VAR$pyp) & STAN.VAR$pyp==1,2])
    ## STAN.VARMON <- as.factor(STAN.VAR[!is.na(STAN.VAR$mon) & STAN.VAR$mon==1,2])
    ## STAN.VARPUB <- as.factor(STAN.VAR[!is.na(STAN.VAR$varpub),2])


    ## STAN.VAR <- read.csv(file = file.path(dbpath, "GitHub", "stan", "inst", "loadDim_var.csv"), na = "") # na = "" for Eurostat column
    ## STAN.VAR <- read.csv(file = file.path(dbpath, "GitHub", "stan", "inst", "extdata", "loadDim_var.csv"))
    STAN.VAR <- read.csv(system.file("extdata", "loadDim_var.csv", package = "stan"))
    ## h(STAN.VAR)
    ## names(STAN.VAR)
    STAN.VARLABEL  <- STAN.VAR[,colnames(STAN.VAR)%in%c("var","label")]
    STAN.VARLABEL$label <- properCase(as.character(STAN.VARLABEL$label))
    STAN.VARLABELFR  <- STAN.VAR[,colnames(STAN.VAR)%in%c("var","labelfr")]
    STAN.VARLABELFR$labelfr <- properCase(as.character(STAN.VARLABELFR$labelfr))
    ## Eurostat
    STAN.VAREUROSTAT  <- STAN.VAR[,colnames(STAN.VAR)%in%c("var","Eurostat")]
    STAN.VAREUROSTAT <- STAN.VAREUROSTAT[!is.na(STAN.VAREUROSTAT[, "Eurostat"]),]
    ## STAN.VAREUROSTAT[, "Eurostat"] <- sub("_", "", STAN.VAREUROSTAT[, "Eurostat"])
    names(STAN.VAREUROSTAT) <- tolower(names(STAN.VAREUROSTAT))

    dim <- "var"
    id <- "Var"
    levels <- STAN.VAR[[dim]]
    namevar <- names(STAN.VAR)[substr(names(STAN.VAR), 1, nchar(id))==id]
    ##
    list.all <- NULL
    ## var <- "VarPYP"
    ## var <- "VarPUB"
    for (var in namevar) {
      if (var=="VarPUB") {        # numbered column
        values <- as.character(STAN.VAR[[dim]][!is.na(STAN.VAR[[var]])])
      } else {
        values <- as.character(STAN.VAR[[dim]][!is.na(STAN.VAR[[var]]) & STAN.VAR[[var]]==1])
      }
      list.var <- list(values)
      names(list.var) <- substr(var, nchar(id)+1, nchar(var))
      list.all <- c(list.all, list.var)
    }
    STAN.VAR <- c(list(levels = levels), list.all)

    STAN.VARALL <- as.factor(STAN.VAR[["ALL"]])
    STAN.VARCLP <- as.factor(STAN.VAR[["CLP"]])
    STAN.VARPYP <- as.factor(STAN.VAR[["PYP"]])
    STAN.VARNSOM <- as.factor(STAN.VAR[["NSOM"]])
    STAN.VARMON <- as.factor(STAN.VAR[["MON"]])
    STAN.VARPUB <- as.factor(STAN.VAR[["PUB"]])
    list <- c(list, "STAN.VARLABEL", "STAN.VARLABELFR", "STAN.VAR", "STAN.VARALL", "STAN.VARCLP", "STAN.VARPYP", "STAN.VARNSOM", "STAN.VARMON", "STAN.VARPUB")
  }

  if ("indi3"%in%dim)
  {
    ## require(xlsx)                   # necessary if formulas are contained in file
    ## STANi3.IND <- read.xlsx(file.path(PATH.SASi3, "lists", "STAN_Industry_list_i3.xls"),1)
    ## STANi3.IND <- STANi3.IND[!colnames(STANi3.IND)%in%c("NA.")]
    ## STANi3.INDLABEL  <- STANi3.IND[,colnames(STANi3.IND)%in%c("Ind","LABEL_en.TEXT")]
    ## names(STANi3.INDLABEL) <- c("ind","label")
    ## STANi3.INDLABEL$label <- properCase(as.character(STANi3.INDLABEL$label))
    ## STANi3.INDALL <- as.factor(STANi3.IND[,"Ind"])

    ## STANi3.INDA6 <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndA6) & STANi3.IND$IndA6==1,3])
    ## STANi3.INDA17 <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndA17) & STANi3.IND$IndA17==1,3])
    ## STANi3.INDA31 <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndA31) & STANi3.IND$IndA31==1,3])
    ## STANi3.INDA60 <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndA60) & STANi3.IND$IndA60==1,3])
    ## STANi3.INDICIO <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndICIO),3])
    ## ##
    ## STANi3.INDA18 <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndA18),3])
    ## STANi3.INDA18 <- STANi3.INDA18[order(STANi3.INDA18)]
    ## ##
    ## STANi3.INDA34 <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndA34),3])
    ## STANi3.INDA34 <- STANi3.INDA34[order(STANi3.INDA34)]


    ## STANi3.IND <- read.csv(file = file.path(dbpath, "GitHub", "stan", "inst", "loadDim_indi3.csv"), na = "")
    STANi3.IND <- read.csv(system.file("extdata", "loadDim_indi3.csv", package = "stan"))
    ## names(STANi3.IND)
    STANi3.INDLABEL  <- STANi3.IND[,colnames(STANi3.IND)%in%c("ind","label")]
    STANi3.INDLABEL$label <- properCase(as.character(STANi3.INDLABEL$label))
    ## Eurostat
    STANi3.INDEUROSTAT  <- STANi3.IND[,colnames(STANi3.IND)%in%c("ind","Eurostat")]
    STANi3.INDEUROSTAT <- STANi3.INDEUROSTAT[!is.na(STANi3.INDEUROSTAT[, "Eurostat"]),]
    STANi3.INDEUROSTAT[, "Eurostat"] <- paste0("C", STANi3.INDEUROSTAT[, "Eurostat"])
    STANi3.INDEUROSTAT[, "Eurostat"] <- sub("-", "2", STANi3.INDEUROSTAT[, "Eurostat"])
    names(STANi3.INDEUROSTAT) <- tolower(names(STANi3.INDEUROSTAT))

    dim <- "ind"
    id <- "Ind"
    levels <- STANi3.IND[[dim]]
    nameind <- names(STANi3.IND)[substr(names(STANi3.IND), 1, nchar(id))==id]
    ##
    list.all <- NULL
    ## ind <- "IndA34"
    for (ind in nameind) {
      ## values <- as.character(STANi3.IND[[dim]][!is.na(STANi3.IND[[ind]])])
      ## values <- factor(STANi3.IND[[dim]][!is.na(STANi3.IND[[ind]])], levels = levels)
      values <- as.character(STANi3.IND[[dim]][!is.na(STANi3.IND[[ind]])])
      list.ind <- list(values)
      names(list.ind) <- substr(ind, nchar(id)+1, nchar(ind))
      list.all <- c(list.all, list.ind)
    }
    STANi3.IND <- c(list(levels = levels), list.all)

    STANi3.INDALL <- STANi3.IND[["ALL"]]
    STANi3.INDA6 <- STANi3.IND[["A6"]]
    STANi3.INDA17 <- STANi3.IND[["A17"]]
    STANi3.INDA31 <- STANi3.IND[["A31"]]
    STANi3.INDA60 <- STANi3.IND[["A60"]]
    STANi3.INDICIO <- STANi3.IND[["ICIO"]]
    STANi3.INDA18 <- STANi3.IND[["A18"]]
    STANi3.INDA34 <- STANi3.IND[["A34"]]
    ## STANi3.INDA34All and STANi3.INDA60All generated with hierarchy below

    ## detach("package:xlsx", unload=TRUE)
    list <- c(list, "STANi3.IND", "STANi3.INDLABEL", "STANi3.INDEUROSTAT", "STANi3.INDALL", "STANi3.INDA6", "STANi3.INDA17", "STANi3.INDA31", "STANi3.INDA60", "STANi3.INDICIO", "STANi3.INDA18", "STANi3.INDA34")
  }

  if ("hierarchyi3"%in%dim)
  {
    ## matrix.agg <- file.path(PATH.COUi3, "Aggregation_general_2digit_ISIC3.csv")
    ## matrix.agg <- file.path(dbpath, "GitHub", "stan", "inst", "loadDim_indi3agg.csv")
    matrix.agg <- system.file("extdata", "loadDim_indi3agg.csv", package = "stan")
    ## test <- read.csv(matrix.agg)
    ## test.m <- melt(test, variable.name = "ind")
    ## ## names(test.m)
    ## test.m <- test.m[test.m$value!=0,]
    ## test.m <- test.m[,!colnames(test.m)%in%c("value")]
    ## test.m <- test.m[order(test.m$agg),]
    ## h(test.m)
    nameagg <- read.csv(matrix.agg)[,1]
    ## agg.include <- union(union(union(STANi3.INDA6, STANi3.INDA18), STANi3.INDA34), c("CTOTAL", "C15T37", "C99")) # "C50T74",
    agg.include <- union(union(union(STANi3.INDA6, STANi3.INDA18), STANi3.INDA34), c("CTOTAL", "C01T02", "C10T12", "C13T14", "C17T18", "C15T37", "C99")) # "C50T74",
    agg.include <- union(agg.include, "C65T99") # UNSD SNA
    agg.include <- union(agg.include, "C80T93") # UNData M+N+O
    agg.include <- union(agg.include, "C71T74") # WIOT
    ## cat(paste0(sort(agg.include), "\n"))
    agg.exclude <- setdiff(nameagg, agg.include)
    ## agg.exclude <- c("C20A36", "C10T41", "C27T35", "C50T74X", "C10T74X", "LOTECH", "HMHTECH", "ENERGYP", "NONMAN")
    ## agg.exclude <- c(agg.exclude, "C50T64", "C65T74", "C50T74", "C50T99")
    ## include "C65T99", "HITECH", "ICTMAN", "ICTSER", "MHTECH", "MLTECH"
    STANi3.HIERARCHY <- stan::hierarchy(file = matrix.agg, agg.exclude = agg.exclude)
    STANi3.HIERARCHYINV <- stan::hierarchy(file = matrix.agg, agg.exclude = agg.exclude, parent = TRUE)
    ## STANi3.INDA60All <- stan::hierarchy(file = matrix.agg, agg.exclude = agg.exclude, order = TRUE)
    STANi3.INDAll <- stan::hierarchy(file = matrix.agg, agg.exclude = agg.exclude, order = TRUE)
    STANi3.INDA34All <- STANi3.INDAll[STANi3.INDAll%in%agg.include]
    STANi3.INDA34All <- STANi3.INDA34All[!STANi3.INDA34All%in%c("C01T02", "C10T12", "C13T14", "C17T18")]
    agg.include <- union(agg.include, STANi3.INDA60)
    ## agg.include <- union(agg.include, c("C01T02", "C17T18"))
    STANi3.INDA60All <- STANi3.INDAll[STANi3.INDAll%in%agg.include]

    list <- c(list, "STANi3.HIERARCHY", "STANi3.HIERARCHYINV", "STANi3.INDA34All", "STANi3.INDA60All")
  }

  if ("ISCO08"%in%dim)
  {
    ## cat(paste0('c("', ISCO08, '", "', ISCO08LABEL, '")\n'))
    conv.occ <- rbind.data.frame(c("OC0T9", "Total"),
                                 c("OC0",   "Armed forces occupations"),
                                 c("OC1",   "Managers"),
                                 c("OC2",   "Professionals"),
                                 c("OC3",   "Technicians and associate professionals"),
                                 c("OC4",   "Clerical support workers"),
                                 c("OC5",   "Service and sales workers"),
                                 c("OC6",   "Skilled agricultural, forestry and fishery workers"),
                                 c("OC7",   "Craft and related trades workers"),
                                 c("OC8",   "Plant and machine operators, and assemblers"),
                                 c("OC9",   "Elementary occupations"),
                                 c("OCNA",  "Not elsewhere classified")
                                 )
    ISCO08 <- conv.occ[,2]
    ISCO08LABEL <- conv.occ[,1]
    list <- c(list, "ISCO08", "ISCO08LABEL")
  }

  if ("indi4"%in%dim)
  {

    require(xlsx)                   # necessary if formulas are contained in file
    ## STAN.IND <- readWorksheetFromFile(file = file.path(PATH.SASi4,"lists", "STAN_Industry_list_i4.xls"), sheet = 2)
    ## STANi4.IND <- read.xlsx(file.path(PATH.SASi4, "lists", "STAN_Industry_list_i4.xls"),2)
    ## STANi4.IND <- read.xlsx(file.path(dbpath, "GitHub", "stan", "inst", "extdata", "loadDim_indi4.xls"), sheetIndex = 1, encoding = "UTF-8")
    STANi4.IND <- read.xlsx(system.file("extdata", "loadDim_indi4.xls", package = "stan"), sheetIndex = 1, encoding = "UTF-8")

    STANi4.IND <- STANi4.IND[!colnames(STANi4.IND)%in%c("NA.")]
    ## #############
    ## lookup tables
    ## #############
    ## industry label
    STANi4.INDLABEL  <- STANi4.IND[,colnames(STANi4.IND)%in%c("Ind","LABEL_en.TEXT")]
    names(STANi4.INDLABEL) <- c("ind","label")
    STANi4.INDLABEL$label <- properCase(as.character(STANi4.INDLABEL$label))
    STANi4.INDLABELFR  <- STANi4.IND[,colnames(STANi4.IND)%in%c("Ind","LABEL_fr.TEXT")]
    names(STANi4.INDLABELFR) <- c("ind","labelfr")
    STANi4.INDLABELFR$labelfr <- properCase(as.character(STANi4.INDLABELFR$labelfr))
    ## Eurostat
    STANi4.INDEUROSTAT  <- STANi4.IND[,colnames(STANi4.IND)%in%c("Ind","Eurostat")]
    STANi4.INDEUROSTAT <- STANi4.INDEUROSTAT[!is.na(STANi4.INDEUROSTAT[, "Eurostat"]),]
    STANi4.INDEUROSTAT[, "Eurostat"] <- paste0("D", STANi4.INDEUROSTAT[, "Eurostat"])
    STANi4.INDEUROSTAT[, "Eurostat"] <- sub("-", "2", STANi4.INDEUROSTAT[, "Eurostat"])
    names(STANi4.INDEUROSTAT) <- tolower(names(STANi4.INDEUROSTAT))

    ## STANi4.INDALL <- as.character(STANi4.IND[!is.na(STANi4.IND$IndALL) & STANi4.IND$IndALL==1, 3])
    ## STANi4.INDA10 <- as.character(STANi4.IND[!is.na(STANi4.IND$IndA10) & STANi4.IND$IndA10==1, 3])
    ## STANi4.INDA21 <- as.character(STANi4.IND[!is.na(STANi4.IND$IndA21) & STANi4.IND$IndA21==1, 3])
    ## STANi4.INDA38 <- as.character(STANi4.IND[!is.na(STANi4.IND$IndA38) & STANi4.IND$IndA38==1, 3])
    ## STANi4.INDA64 <- as.character(STANi4.IND[!is.na(STANi4.IND$IndA64) & STANi4.IND$IndA64==1, 3])
    ## STANi4.INDA88 <- as.character(STANi4.IND[!is.na(STANi4.IND$IndA88) & STANi4.IND$IndA88==1, 3])
    ## STANi4.INDICIO <- as.character(STANi4.IND[!is.na(STANi4.IND$IndICIO), 3])

    dim <- "Ind" ## ISIC3 : ind lower case
    id <- "Ind"
    levels <- STANi4.IND[[dim]]
    nameind <- names(STANi4.IND)[substr(names(STANi4.IND), 1, nchar(id))==id]
    ## names(STANi4.IND)
    ##
    list.all <- NULL
    ## ind <- "IndA10"
    ## ind <- "IndPUB"
    for (ind in nameind[!nameind%in%c("Ind", "Ind4", "IndMANUF", "IndICT", "IndEUK")]) {
      ## values <- as.character(STANi4.IND[[dim]][!is.na(STANi4.IND[[ind]])])
      if (ind=="IndPUB") {
        values <- as.character(STANi4.IND[[dim]][!is.na(STANi4.IND[[ind]])])
      } else {
        values <- as.character(STANi4.IND[[dim]][!is.na(STANi4.IND[[ind]]) & STANi4.IND[[ind]]==1])
      }
      list.ind <- list(values)
      names(list.ind) <- substr(ind, nchar(id)+1, nchar(ind))
      list.all <- c(list.all, list.ind)
    }
    STANi4.IND <- c(list(levels = levels), list.all)

    STANi4.INDALL <- STANi4.IND[["ALL"]]
    STANi4.INDPUB <- STANi4.IND[["PUB"]]
    STANi4.INDA10 <- STANi4.IND[["A10"]]
    STANi4.INDA21 <- STANi4.IND[["A21"]]
    STANi4.INDA38 <- STANi4.IND[["A38"]]
    STANi4.INDA64 <- STANi4.IND[["A64"]]
    STANi4.INDA88 <- STANi4.IND[["A88"]]
    STANi4.INDICIO <- STANi4.IND[["ICIO"]]

    detach("package:xlsx", unload=TRUE)
    list <- c(list, "STANi4.INDLABEL", "STANi4.INDLABELFR", "STANi4.INDEUROSTAT", "STANi4.IND", "STANi4.INDALL", "STANi4.INDPUB", "STANi4.INDA10", "STANi4.INDA21", "STANi4.INDA38", "STANi4.INDA64", "STANi4.INDA88", "STANi4.INDICIO")
  }

  if ("hierarchyi4"%in%dim)
  {
    ## ## matrix.agg <- file.path(PATH.COUi4, "Aggregation_general_2digit.csv")
    ## matrix.agg <- system.file("extdata", "loadDim_indi4agg.csv", package = "stan")
    ## agg.exclude <- c("D05T39", "D45T82", "D45T99", "D45T82X", "D05T82X", "D16A31", "D24T33X", "ENERGYP", "NONMAN")
    ## ## include "C10T41", "C65T99", "HITECH", "ICTMAN", "ICTSER", "MHTECH", "MLTECH"
    ## ## included: "C10T41", "C50T64", "C65T74", "C50T74", "C50T99"
    ## STANi4.HIERARCHY <- hierarchy(file = matrix.agg, agg.exclude = agg.exclude)
    ## STANi4.HIERARCHYINV <- hierarchy(file = matrix.agg, agg.exclude = agg.exclude, parent = TRUE)
    ## list <- c(list, "STANi4.HIERARCHY", "STANi4.HIERARCHYINV")

    matrix.agg <- system.file("extdata", "loadDim_indi4agg.csv", package = "stan")
    nameagg <- read.csv(matrix.agg)[,1]

    agg.include <- union(
      union(
        union(
          union(
            union(STANi4.INDA10, STANi4.INDA21),
            STANi4.INDA38
          ),
          STANi4.INDA64
        ),
        STANi4.INDA88
      ),
      c("DTOTAL", "D01T02", "D05T39", "D05T06", "D07T08", "D10T11", "D13T14", "D20T21", "D19T23", "D26T27", "D26T28", "D35T39", "D68T82", "D90T96", "D84T99", "D45T82") # D45T99
    )

    agg.exclude <- setdiff(nameagg, agg.include)
    STANi4.HIERARCHY <- stan::hierarchy(file = matrix.agg, agg.exclude = agg.exclude)
    STANi4.HIERARCHYINV <- stan::hierarchy(file = matrix.agg, agg.exclude = agg.exclude, parent = TRUE)

    STANi4.INDAll <- stan::hierarchy(file = matrix.agg, agg.exclude = agg.exclude, order = TRUE)
    ## STANi4.INDA34All <- STANi4.INDAll[STANi4.INDAll%in%agg.include]
    ## STANi4.INDA34All <- STANi4.INDA34All[!STANi4.INDA34All%in%c("C01T02", "C10T12", "C13T14", "C17T18")]
    agg.include <- union(agg.include, STANi4.INDA88)
    STANi4.INDA88All <- STANi4.INDAll[STANi4.INDAll%in%agg.include]
    STANi4.INDA88All <- unlist(STANi4.INDA88All)

    list <- c(list, "STANi4.HIERARCHY", "STANi4.HIERARCHYINV", "STANi4.INDA88All")
  }

  if ("cur"%in%dim)
  {

    require(XML)
    require(XLConnect)
    require(RCurl)
    proxy <- Sys.getenv("_http_proxy")
    ## proxy <- "wsg-proxy.oecd.org:80"
    ## proxy <- ""
    curl <- getCurlHandle()
    curlSetOpt(.opts = list(proxy = proxy), curl = curl)

    ## ## United Nations, Countries or areas, codes and abbreviations
    ## url.UN <- rbind.data.frame(c("en", "http://unstats.un.org/unsd/methods/m49/m49alpha.htm"),
    ##                            c("fr", "http://unstats.un.org/unsd/methods/m49/m49alphaf.htm"))
    ## ##
    ## table.UN.all <- NULL
    ## ## i <- 2
    ## for (i in seq(along=url.UN))
    ##     {
    ##         ## table.UN <- readHTMLTable(as.character(url.UN[i,2]), encoding = "utf-8")
    ##         tt <- getURL(as.character(url.UN[i,2]), curl = curl)
    ##         table.UN <- htmlParse(tt, encoding = "utf-8")
    ##         table.UN <- readHTMLTable(table.UN, encoding = "utf-8")
    ##         table.UN <- table.UN[[4]]
    ##         firstrow <- match("004", table.UN[,1])
    ##         ## table.UN <- table.UN[-c(1:19),]
    ##         table.UN <- table.UN[-c(1:(firstrow-1)),]
    ##         ## h(table.UN)
    ##         names(table.UN) <- c("codeUN", "countryUN", "cou")
    ##         ## h(table.UN)
    ##         table.UN <- table.UN[length(table.UN[,1]) > 3 & !is.na(table.UN[,3]),]
    ##         table.UN$countryUN <- gsub("\r\n", " ", table.UN$countryUN)
    ##         table.UN$countryUN <- gsub("[ ]+", " ", table.UN$countryUN)
    ##         table.UN$countryUN <- sub(intToUtf8(160), " ", table.UN$countryUN)

    ##         table.UN$lang <- url.UN[i,1]
    ##         table.UN.all <- rbind(table.UN.all, table.UN)
    ##     }

    ## table.UN <- (merge(table.UN.all[table.UN.all$lang=="en",],
    ##                    table.UN.all[table.UN.all$lang=="fr",], by = "cou"))
    ## names(table.UN) <- sub("countryUN.x", "countryUNen", names(table.UN))
    ## names(table.UN) <- sub("countryUN.y", "countryUNfr", names(table.UN))
    ## names(table.UN) <- sub("codeUN.x", "codeUN", names(table.UN))
    ## table.UN <- subset(table.UN, select = c("cou", "countryUNen", "countryUNfr", "codeUN"))
    ## table.UN <- table.UN[!table.UN$cou=="",]
    ## file.UN <- file.path(dbpath, "GitHub", "stan", "inst", "extdata", "loadDim_cur_UNCountries.csv")
    ## write.csv(table.UN, file = file.UN, row.names = FALSE)

    ## replace long country labels
    file.UN <- system.file("extdata", "loadDim_cur_UNCountries.csv", package = "stan")
    table.UN <- read.csv(file = file.UN)
    table.UN$countryUNen <- as.character(table.UN$countryUNen)
    table.UN$countryUNfr <- as.character(table.UN$countryUNfr)
    ## View(table.UN)
    STAN.COUUN <- subset(table.UN, select = c("cou", "codeUN"))

    replace.cou.file <- system.file("extdata", "loadDim_cur_UNCountries_replace.csv", package = "stan")
    replace.cou <- read.csv(file = replace.cou.file)
    replace.cou$countryUNen <- as.character(replace.cou$countryUNen)
    replace.cou$countryUNfr <- as.character(replace.cou$countryUNfr)
    ## cou <- replace.cou$cou[1]
    for (cou in as.character(replace.cou$cou)) {
      table.UN$countryUNen[table.UN$cou==cou] <- replace.cou$countryUNen[replace.cou$cou==cou]
      table.UN$countryUNfr[table.UN$cou==cou] <- replace.cou$countryUNfr[replace.cou$cou==cou]
    }
    ## subset(table.UN, cou=="KOR")

    ## ## nationsonline.org
    ## url.NO <- "http://www.nationsonline.org/oneworld/country_code_list.htm"
    ## ## table.NO <- readHTMLTable(url.NO)
    ## tt <- getURL(url.NO, curl = curl)
    ## table.NO <- htmlParse(tt)
    ## table.NO <- readHTMLTable(table.NO)
    ## table.NO <- table.NO[[4]]
    ## table.NO <- table.NO[-1,]
    ## ## h(table.NO)
    ## names(table.NO) <- c("flagNO", "countryNO", "cou2", "cou", "codeNO")
    ## file.NO <- file.path(dbpath, "GitHub", "stan", "inst", "extdata", "loadDim_cur_NationsOnline.csv")
    ## write.csv(table.NO, file = file.NO, row.names = FALSE)

    file.NO <- system.file("extdata", "loadDim_cur_NationsOnline.csv", package = "stan")
    read.csv(file = file.NO)

    ## merge UN and NO
    table.UN.NO <- merge(table.UN, table.NO)
    ## ISO 3166 country names (for check) - currently not available 02/10/2104
    ## url.ISO3166 <- "http://www.iso.org/iso/home/standards/country_codes/country_names_and_code_elements_txt.htm"
    ## file.ISO3166 <- file.path(PATH.SASi3, "DATA_in", "ISO", "country_names_and_code_elements_txt-temp.txt")
    ## file.ISO3166 <- file.path(dbpath, "GitHub", "stan", "inst", "extdata", "loadDim_cur_ISO3166_utf8.txt")

    file.ISO3166 <- system.file("extdata", "loadDim_cur_ISO3166_utf8.txt", package = "stan")
    table.ISO3166 <- readLines(file.ISO3166, encoding = "UTF-8")
    ##
    X <- strsplit(table.ISO3166, split = ";")
    X <- X[sapply(X, length) > 0]
    table.ISO3166 <- matrix("", nrow=length(X), ncol=2)
    for (i in seq(along=X)) {
      table.ISO3166[i,1] <- X[[i]][1]
      table.ISO3166[i,2] <- X[[i]][2]
    }
    table.ISO3166 <- as.data.frame(table.ISO3166)
    table.ISO3166 <- table.ISO3166[-1,]
    names(table.ISO3166) <- c("countryISO", "cou2")
    table.UN.NO.ISO3166 <- merge(table.UN.NO, table.ISO3166)

    ## ## ISO 4217 from xls link
    ## file.ISO4217 <- tempfile(fileext = ".xls")
    ## ##
    ## ## download.file("http://www.currency-iso.org/dam/downloads/table_a1.xls", destfile = file.ISO4217, method = "curl")
    ## download.file("http://www.currency-iso.org/dam/downloads/table_a1.xls", destfile = file.ISO4217, mode="wb")
    ## table.ISO4217 <- readWorksheetFromFile(file.ISO4217, sheet = 1, startRow = 4)
    ## names(table.ISO4217) <- c("countryISO", "currency", "cur", "curcode", "unitISO", "fundISO")
    ## file.ISO4217 <- file.path(dbpath, "GitHub", "stan", "inst", "extdata", "loadDim_cur_ISO4217.csv")
    ## write.csv(table.ISO4217, file = file.ISO4217)

    file.ISO4217 <- system.file("extdata", "loadDim_cur_ISO4217.csv", package = "stan")
    table.ISO4217 <- read.csv(file = file.ISO4217)

    table.UN.NO.ISO3166.ISO4217 <- merge(table.UN.NO.ISO3166, table.ISO4217)

    ## ## Eurostat names, codes and protocol order
    ## ## not available 05.03.2015
    ## ## require(RCurl)
    ## url.EU <- "http://publications.europa.eu/code/pdf/370000en.htm"
    ## ## curl <- getCurlHandle()
    ## agent <- "Mozilla/5.0"
    ## curlSetOpt(cookiejar="cookiesk.txt",  useragent = agent, followlocation = TRUE, curl=curl)
    ## tt <- getURL(url.EU, curl = curl)
    ## table.EU <- readHTMLTable(tt, encoding = "utf-8")
    ## table.EU <- table.EU[[3]]
    ## table.EU <- table.EU[-1,]
    ## names(table.EU) <- c("countryEUnat", "countryEUen", "countryEUoff", "cou2EU", "former")
    ## table.EU$cou2 <- table.EU$cou2EU
    ## table.EU$cou2 <- sub("EL", "GR", table.EU$cou2)
    ## table.EU$cou2 <- sub("UK", "GB", table.EU$cou2)
    ## table.EU$countryEUen <- gsub("\r\n", "", table.EU$countryEUen)
    ## table.EU$countryEUoff <- gsub("\r\n", "", table.EU$countryEUoff)
    ## table.EU <- table.EU[,!colnames(table.EU)%in%c("countryEUnat", "former")]
    ## table.EU <- table.EU[!is.na(table.EU$cou2EU),]
    ## table.EU$inEURO <- 1
    ## ##
    ## table.UN.NO.ISO3166.ISO4217.EU <- merge(table.UN.NO.ISO3166.ISO4217, table.EU, by = c("cou2"), all = TRUE)

    ## table <- table.UN.NO.ISO3166.ISO4217.EU
    table <- table.UN.NO.ISO3166.ISO4217
    ## table$inEURO[is.na(table$inEURO)] <- 0
    ##
    STAN.COUCUR <- subset(table[is.na(table$fundISO) & !is.na(table$cur),], select = c("cou", "cur"))
    STAN.COUCUR$cou <- as.character(STAN.COUCUR$cou)
    STAN.COUCUR <- rbind.data.frame(STAN.COUCUR, c("TWN", "TWD"))
    STAN.COUCUR$cou <- as.factor(STAN.COUCUR$cou)
    ##
    ## STAN.COUEN <- subset(table, select = c("cou", "countryUNen"), is.na(table$fundISO) & !is.na(table$cur))
    ## ## STAN.COUEN[STAN.COUEN$cou=="GBR", ]
    ## STAN.COUEN$countryUNen <- gsub(" of Great Britain and Northern Ireland", "", STAN.COUEN$countryUNen, fixed = TRUE) # United Kingdom
    ## STAN.COUEN$countryUNen <- gsub(" (Plurinational State of)", "", STAN.COUEN$countryUNen, fixed = TRUE) # Bolivia
    ## STAN.COUEN$countryUNen <- gsub(" (Bolivarian Republic of)", "", STAN.COUEN$countryUNen, fixed = TRUE) # Venezuela
    ## STAN.COUEN$countryUNen <- gsub(" (Islamic Republic of)", "", STAN.COUEN$countryUNen, fixed = TRUE) # Iran
    ## STAN.COUEN$cou <- as.character(STAN.COUEN$cou)
    ## STAN.COUEN <- rbind.data.frame(STAN.COUEN, c("TWN", "Chinese Taipei"))
    ## STAN.COUEN$cou <- as.factor(STAN.COUEN$cou)
    ##
    STAN.COUFR <- subset(table[is.na(table$fundISO) & !is.na(table$cur),], select = c("cou", "countryUNfr"))
    STAN.COUFR$cou <- as.character(STAN.COUFR$cou)
    STAN.COUFR$countryUNfr <- as.character(STAN.COUFR$countryUNfr)
    STAN.COUFR <- rbind.data.frame(STAN.COUFR, c("TWN", "Taipei Chinois"))
    STAN.COUFR$cou <- as.factor(STAN.COUFR$cou)
    ##
    STAN.COU2 <- subset(table[is.na(table$fundISO) & !is.na(table$cur),], select = c("cou", "cou2"))
    ## STAN.COUEU <- subset(table[table$inEURO==1,], select = c("cou"))[,1]
    ## STAN.COUUN <- subset(table[is.na(table$fundISO) & !is.na(table$cur),], select = c("cou", "codeUN")) # see above
    ## list <- list[!list%in%c("STAN.CUR")]
    ## rm(STAN.CUR)

    list <- c(list, "STAN.COUCUR", "STAN.COUFR", "STAN.COU2", "STAN.COUUN") # "STAN.COUEN"
  }
  if ("unreg"%in%dim)
  {

    require(XML)
    require(XLConnect)
    require(RCurl)
    require(magrittr)
    proxy <- Sys.getenv("_http_proxy")
    ## proxy <- "wsg-proxy.oecd.org:80"
    ## proxy <- ""
    curl <- getCurlHandle()
    curlSetOpt(.opts = list(proxy = proxy), curl = curl)

    ## United Nations, Composition of macro geographical (continental) regions, geographical sub-regions, and selected economic and other groupings
    url_UN <- "http://unstats.un.org/unsd/methods/m49/m49regin.htm"
    ##
    ## table_UN <- readHTMLTable(as.character(url_UN[i,2]), encoding = "utf-8")
    ## tt <- getURL(as.character(url_UN[i,2]), curl = curl)
    tt <- getURL(as.character(url_UN), curl = curl)
    table_UN <-
      htmlParse(tt, encoding = "utf-8") %>%
      readHTMLTable(., encoding = "utf-8") %>%
      .[[4]]

    ## table_UN <- htmlParse(tt, encoding = "utf-8")
    ## table_UN <- readHTMLTable(table_UN, encoding = "utf-8")
    ## table_UN <- table_UN[[4]]
    ## firstrow <- match("004", table_UN[,1])

    ## keep 1st part of table with world and regions
    ## rownum <- nrow(table_UN)
    ## firstrow <- match("001 World", table_UN[,1])
    ## lastrow <- match("722 Small island developing states", table_UN[,1])
    ## deleterow <- c(1:(firstrow-1),
    ##                lastrow+1:rownum)

    ## keep 2nd part of table with countries
    firstrow <- match("002", table_UN[,1])
    lastrow <- match("876", table_UN[,1])
    deleterow <- c(1:(firstrow-1),
                   lastrow+1:rownum)

    ## table_UN <- table_UN[-c(1:19),]
    ## nrow(table_UN_bottom)
    table_UN_bottom <-
      ## table_UN[-c(1:(firstrow-1)),]
      table_UN[-deleterow,]

    ## table_UN_bottom <- table_UN[-deleterow,]
    ## h(table_UN_bottom)
    names(table_UN_bottom) <- c("codeUN", "countryUN")
    ## h(table_UN_bottom)

    ## clean
    table_UN_bottom_clean <-
      table_UN_bottom %>%
      .[.[["countryUN"]] != "",] # remove rows without label

    ## change factors to integers
    table_UN_bottom_clean[["codeUN"]] <- as.integer(as.character(table_UN_bottom_clean[["codeUN"]]))

    table_UN_bottom_clean[["countryUN"]] <- gsub("\r\n", " ", table_UN_bottom_clean[["countryUN"]])
    table_UN_bottom_clean[["countryUN"]] <- gsub("[ ]+", " ", table_UN_bottom_clean[["countryUN"]])
    table_UN_bottom_clean[["countryUN"]] <- gsub("( [a-z]/)", "", table_UN_bottom_clean[["countryUN"]]) # " a/", " b/" etc.

    ## join ISO-3 country codes on UN codes to identify regions (those without an ISO-3 country code)
    file_cur_UNCountries <- system.file("extdata", "loadDim_cur_UNCountries.csv", package = "stan")
    table_cur_UNCountries <-
      read.csv(file = file_cur_UNCountries, stringsAsFactors = FALSE) %>%
      .[ , c("codeUN", "cou")]

    file_UN <- file.path(dbpath, "GitHub", "stan", "inst", "extdata", "loadDim_regionUNen.tsv")
    write.table(table_UN_bottom_clean, file = file_UN, quote=FALSE, row.names = FALSE, sep = "\t", na = "")

    
  }

  if ("hierarchyCOUUN"%in%dim)
  {
    matrix.agg <- system.file("extdata", "loadDim_regionUNagg.csv", package = "stan")
    nameagg <- read.csv(matrix.agg)[,1]
    ## agg.include <- union(union(union(STANi3.INDA6, STANi3.INDA18), STANi3.INDA34), c("CTOTAL", "C01T02", "C10T12", "C13T14", "C17T18", "C15T37", "C99")) # "C50T74",
    ## agg.include <- union(agg.include, "C65T99") # UNSD SNA
    ## agg.include <- union(agg.include, "C80T93") # UNData M+N+O
    ## agg.include <- union(agg.include, "C71T74") # WIOT
    ## ## cat(paste0(sort(agg.include), "\n"))
    ## agg.exclude <- setdiff(nameagg, agg.include)
    ## STANi3.HIERARCHY <- stan::hierarchy(file = matrix.agg, agg.exclude = agg.exclude)
    STANCOUUN.HIERARCHY <- stan::hierarchy(file = matrix.agg, agg.exclude = "", colnameprefix = "UN")
    STANCOUUN.HIERARCHYINV <- stan::hierarchy(file = matrix.agg, agg.exclude = "", parent = TRUE, colnameprefix = "UN")
    ## STANi3.INDA60All <- stan::hierarchy(file = matrix.agg, agg.exclude = agg.exclude, order = TRUE)

    list <- c(list, "STANCOUUN.HIERARCHY", "STANCOUUN.HIERARCHYINV")
  }

  save(list = list, file = file)
  if (datalist==TRUE) addDatalist(file = file, list = list)
}
