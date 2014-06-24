#' SAS batch
#'
#' Create SAS batch programs
#'
#' Create or modify SAS batch program for STAN based on a list of countries.
#'
#' @param cou a 3-digit country ISO code character string.
#' @param isic an integer to specify the version of STAN.
#' @param prog.sas a character vector with combination of \code{load}, \code{estim}, \code{pub}.
#' @param quiet a logical to return the created SAS program contents.
#'
#' @author OECD STAN
#' @keywords SAS
#' @seealso \code{\link{couCreate}}, \code{\link{SASload}}, \code{\link{SASestim}}, \code{\link{SASpub}}
#' @export
#' @examples
#' SASbatch(namecou=c("BGR"), prog.sas="load")

SASbatch <- function(namecou=c("BGR", "LVA"),
                     isic=4,
                     prog.sas=c("load", "estim", "pub"),
                     quiet=FALSE,
                     export=TRUE)
{
    if (isic==3)
    {
        path.sas <- PATH.SASi3
        path.cou <- PATH.COUi3
    }
    if (isic==4)
    {
        path.sas <- PATH.SASi4
        path.cou <- PATH.COUi4
    }
    ##
    prog.batch <- paste0(path.sas, "progs\\PAYS\\batchCou_", gsub(", ", "-", toString(prog.sas)), ".sas")
    fileCon <- file(prog.batch)
    if (!file.exists(prog.batch)) file.create(prog.batch)
    prepend <- paste0("\n",
                      "/* %INCLUDE \"", path.sas, "progs\\0_Executable.sas\" ; */\n",
                      "\n",
                      "%MACRO batchCou;\n",
                      "\n",
                      "/* Ferme la librairie de l'ancien pays actif */")
    append <- paste0("\n\tLIBNAME &cou CLEAR ;\n",
                     "\n",
                     "%END;\n",
                     "\n",
                     "%MEND batchCou;\n",
                     "%batchCou;\n")

    if (export==TRUE)
    {
        append <- paste0("\tPROC EXPORT DATA=&cou..swn OUTFILE=\"", path.cou, "&cou.\\SASdata\\swn.csv\" REPLACE; RUN ;\n", append)
    }

    list.cou.string <- gsub(", ", " ", toString(namecou))
    lines.prog <- rbind.data.frame(c(TRUE, "LIST_COU", paste0("/* %INCLUDE \"", path.sas, "progs\\LISTS\\load STAN_cou.sas\" ; */")),
                                   c(TRUE, "LIST_UNIT", paste0("/* %INCLUDE \"", path.sas, "progs\\LISTS\\load STAN_unit.sas\" ; */\n")),
                                   c(TRUE, "UPDATE_NAMA", paste0("/* %updateNAMA ; */\n")),
                                   c(TRUE, "LIBNAME_CLOSE", "LIBNAME &cou CLEAR ;"),
                                   c(TRUE, "LIBNAMEi3_CLOSE", "/* LIBNAME &cou._i3 CLEAR ; */"),
                                   c(TRUE, "COUNTRIES", paste0("\n%LET countries=", list.cou.string, " ;\n")),
                                   c(TRUE, "LOOP", "%DO f=0 %TO %NWORD(&countries) ;\n"),
                                   c(TRUE, "ASSIGN_COU", "%LET cou=%SCAN(&countries,&f,\" \") ;\n"),
                                   c(TRUE, "LIBNAME_NEW", paste0("\tLIBNAME &cou \"", path.cou, "&cou.\\SASdata\" ;\n")),
                                   c(FALSE, "LOAD", paste0("\t%INCLUDE \"", path.sas, "progs\\PAYS\\&cou.\\&cou._load.sas\" ;\n")),
                                   c(FALSE, "ESTIM", paste0("\t%INCLUDE \"", path.sas, "progs\\PAYS\\&cou.\\&cou._estim.sas\" ;\n")),
                                   c(FALSE, "PUB", paste0("\t%INCLUDE \"", path.sas, "progs\\PAYS\\&cou.\\&cou._pub.sas\" ;\n")))
    names(lines.prog) <- c("fixed", "line", "sascode")
    body <- lines.prog$sascode[lines.prog$fixed==TRUE | lines.prog$line%in%toupper(prog.sas)]
    text <- c(prepend, paste0(body), append)
    writeLines(text = text, con = fileCon)
    close(fileCon)
    if (quiet==FALSE) return(text)
}
