#' SAS pub
#'
#' Create SAS publication programs
#'
#' Create or modify SAS publication programs for STAN based on a list of countries and sources.
#'
#' @param cou a 3-digit country ISO code character string.
#' @param isic an integer to specify the version of STAN.
#' @param del.ind a character vector naming the industries to remove before publication.
#' @param pub.ind a character vector restricting the industries to publish.
#' @param quiet a logical to return the created SAS program contents.
#'
#' @author OECD STAN
#' @keywords SAS
#' @seealso \code{\link{SASbatch}}, , \code{\link{SASload}}, \code{\link{SASestim}}
#' @export
#' @examples
#' SASpub(cou="BGR", sources=list(BGR = c('NSONA', 'SUT', 'ISPATCH')))

SASpub <- function(cou=stop("'cou' must be specified"),
                   isic=3,
                   pub.ind=NULL,
                   del.ind=NULL,
                   quiet=FALSE)
{
    if (isic==3)
    {
        path.sas <- PATH.SASi3
        if (is.null(pub.ind)) pub.ind=STANi3.INDICIO
    }
    if (isic==4)
    {
        path.sas <- PATH.SASi4
        if (is.null(pub.ind)) pub.ind=STANi4.INDICIO
    }
    ##
    prog.cou <- paste0(path.sas, "progs\\PAYS\\", cou, "\\", cou, "_pub.sas")
    fileCon <- file(prog.cou)
    if (!file.exists(prog.cou)) file.create(prog.cou)
    prepend <- paste0("/****************************************************************/\n",
                      "/* Sauvegarde des donnees publiables + Exportation des donnees  */\n",
                      "/****************************************************************/\n",
                      "\n",
                      "DATA pub; SET &cou..swn (WHERE=(sou='NAPATCH')); RUN;\n")
    append <- paste0("\n",
                     "\n",
                     "/****************************************************/\n",
                     "%LISTE(stan_sas.stan_ind,ind,%STR(digitInd=\"0\"),1);\n",
                     "%LISTE(stan_sas.stan_var,var,%STR(varPUB^=.),1);\n",
                     "DATA pub; SET pub;\n",
                     "	IF ind IN (&list_ind) THEN note=\"AGG\";\n",
                     "	IF var IN (&list_var);\n",
                     "RUN;\n",
                     "/****************************************************/\n",
                     "\n",
                     "/* Calcul des arrondis */\n",
                     "DATA pub; SET pub;\n",
                     "	IF var IN ('PRDP','VALP','INTP','GFCP') THEN value=ROUND(value,0.00001);\n",
                     "	ELSE value=ROUND(value);\n",
                     "RUN;\n",
                     "\n",
                     "/* PUB ==> SWN */\n",
                     "DATA &cou..swn; SET &cou..swn; IF sou=\"PUB\" THEN DELETE; RUN;\n",
                     "PROC SQL; INSERT INTO &cou..swn SELECT \"PUB\" AS sou,var,ind,year,value,note FROM pub; QUIT;\n",
                     "\n",
                     "\n",
                     "/*\n",
                     "%SaveExport;\n",
                     "*/\n",
                     "%SaveExportPRE;\n",
                     "\n")
    list.ind <- setdiff(pub.ind, del.ind)
    list.ind.string <- gsub(", ", "-", toString(list.ind))
    lines.prog <- rbind.data.frame(c(TRUE, "KEEP", paste0("%eDEL_row (pub,,,!", list.ind.string, ",,!*-*ANA-*HS1-*NSO-*SUT);")))
    names(lines.prog) <- c("fixed", "line", "sascode")
    body <- lines.prog$sascode[lines.prog$fixed==TRUE | lines.prog$line%in%list.ind]
    text <- c(prepend, paste0(body), append)
    writeLines(text = text, con = fileCon)
    close(fileCon)
    if (quiet==FALSE) return(text)
}
