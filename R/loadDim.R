#' Load Dimensions
#'
#' Load STAN dimension members
#'
#' Function to create a data file with the dimension members used in STAN.
#'
#' @param dim a list of dimension to be loaded.
#' @param file a data file
#' @param datalist a boolean expression if the file and its contents shall be added to a package data list
#'
#' @author OECD STAN
#' @keywords dimensions
#' @seealso \code{\link{indAggregate}}
#' @export
#' @examples
#' loadDim(dim = c("cou", "var"), file = "data.rda", datalist = TRUE)

loadDim <- function(dim=c("cou", "var", "indi4", "indi3"),
                    file=paste0(PATH.REPO,'stan\\data\\stanDim.rda'),
                    datalist=TRUE,
                    ...)
{
    require(XLConnect)
    require(ifultools)
    list <- NULL
    if ("cou"%in%dim)
    {
        STAN.COU <- readWorksheetFromFile(file = paste0(PATH.SASi4, "lists/STAN_cou_list.xls"), sheet = 1)
        names(STAN.COU) <- tolower(names(STAN.COU))
        STAN.COU2 <- STAN.COU$iso2[STAN.COU$inoecd==1]
        STAN.COU <- STAN.COU$cou[STAN.COU$inoecd==1]
        list <- c(list, "STAN.COU", "STAN.COU2")
    }
    if ("var"%in%dim)
    {
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
    if ("indi3"%in%dim)
    {
        require(xlsx)                   # necessary if formulas are contained in file
        STANi3.IND <- read.xlsx(paste0(PATH.SASi3,'lists/STAN_Industry_list_i3.xls'),1)
        STANi3.IND <- STANi3.IND[!colnames(STANi3.IND)%in%c('NA.')]
        STANi3.INDLABEL  <- STANi3.IND[,colnames(STANi3.IND)%in%c('Ind','LABEL_en.TEXT')]
        names(STANi3.INDLABEL) <- c('ind','label')
        STANi3.INDLABEL$label <- properCase(as.character(STANi3.INDLABEL$label))
        STANi3.INDALL <- as.factor(STANi3.IND[,3])
        STANi3.INDA6 <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndA6) & STANi3.IND$IndA6==1,3])
        STANi3.INDA17 <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndA17) & STANi3.IND$IndA17==1,3])
        STANi3.INDA31 <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndA31) & STANi3.IND$IndA31==1,3])
        STANi3.INDA60 <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndA60) & STANi3.IND$IndA60==1,3])
        STANi3.INDICIO <- as.factor(STANi3.IND[!is.na(STANi3.IND$IndICIO),3])
        detach("package:xlsx", unload=TRUE)
        list <- c(list, "STANi3.INDLABEL", "STANi3.INDALL", "STANi3.INDA6", "STANi3.INDA17", "STANi3.INDA31", "STANi3.INDA60", "STANi3.INDICIO")
    }
    save(list = list, file = file)
    if (datalist==TRUE) addDatalist(file = file, list = list)
}
