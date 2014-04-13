#' COU List
#'
#' Updates country list
#'
#' Adds or removes country from SAS Country List with national currency.
#'
#' @param cou a 3-digit country ISO code character string.
#' @param isic an integer to specify the version of STAN.
#'
#' @author OECD STAN
#' @keywords SAS
#' @seealso \code{\link{couCreate}}
#' @export
#' @examples
#' couList(namecou=c("LVA"))

couList <- function(namecou=c("LVA"),
                    isic=4)
{
    if (isic==3) namefile=paste0(PATH.SASi3, "Lists\\STAN_cou_list.csv")
    if (isic==4) namefile=paste0(PATH.SASi4, "Lists\\STAN_cou_list.csv")
    ##
    cou.list <- read.csv(namefile)
    cou.list <- cou.list[!cou.list$Cou%in%namecou,]
    for (cou in namecou)
    {
        ## cou <- namecou[1]
        if (cou%in%STAN.COUEU) inEURO = 1 else inEURO = 0
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
