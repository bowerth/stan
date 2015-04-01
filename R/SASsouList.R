#' SAS SOU List
#'
#' Updates sources list
#'
#' Adds or removes sources from SAS Source List.
#'
#' @param sou a source code character string.
#' @param isic an integer to specify the version of STAN.
#'
#' @author OECD STAN
#' @keywords SAS
#' @seealso \code{\link{SAScouCreate}}, \code{\link{SAScouList}}
#' @export
#' @examples
#' souList(namesou=c("LVA"))

SASsouList <- function(namesou=c("STANandBTD"),
                       isic=4)
{
    if (isic==3) namefile = file.path(PATH.SASi3, "Lists", "STAN_sou_list.csv")
    if (isic==4) namefile = file.path(PATH.SASi4, "Lists", "STAN_sou_list.csv")
    ##
    sou.list <- read.csv(namefile)
    sou.list <- sou.list[!sou.list$Sou%in%namesou,]
    for (sou in namesou)
    {


        ## sou <- namesou[1]
        if (sou%in%STAN.COUEU) inEURO = 1 else inEURO = 0
        cou.new <- cbind.data.frame(couPUB = (max(cou.list$couPUB) + 1),
                                    Cou = cou,
                                    LbCou_en = STAN.COUEN[STAN.COUEN$cou==cou,2],
                                    LbCou_fr = STAN.COUFR[STAN.COUFR$cou==cou,2],
                                    inOECD = 0,
                                    inEU = 0,
                                    inEURO = inEURO ##,
                                    ## Cur = STAN.CUR[STAN.CUR$cou==cou,2]
                                    )
        cou.list <- rbind(cou.list, cou.new)
    }
    return(cou.list)
}
