#' SAS estim
#'
#' Create SAS estimation programs
#'
#' Create or modify SAS estimation programs for STAN based on a list of countries and sources.
#'
#' @param cou a 3-digit country ISO code character string.
#' @param isic an integer to specify the version of STAN.
#' @param sources a list naming the sources used for estimation.
#' @param variables a character vector naming the variables to estimate.
#' @param period a character string with the estimation period.
#' @param quiet a logical to return the created SAS program contents.
#'
#' @author OECD STAN
#' @keywords SAS
#' @seealso \code{\link{SASbatch}}, \code{\link{SASload}}, \code{\link{SASpub}}
#' @export
#' @examples
#' SASestim(cou="BGR", sources=list(BGR = c('NSONA', 'SUT', 'ISPATCH')))

SASestim <- function(cou=stop("'cou' must be specified"),
                    isic=4,
                    sources=list(BGR = c('NSO', 'SUT', 'ISPATCH')),
                    variables=c("PROD", "VALU", "LABR"),
                    period="1970-2010",
                    quiet=FALSE)
{
    if (isic==3) {
        path.sas <- PATH.SASi3
        add.zero <- c("C95")
    }
    if (isic==4) {
        path.sas <- PATH.SASi4
        add.zero <- c("D99")
    }
    ##
    prog.cou <- file.path(path.sas, "progs", "PAYS", cou, paste0(cou, "_estim.sas"))
    fileCon <- file(prog.cou)
    if (!file.exists(prog.cou)) file.create(prog.cou)
    list.var <- gsub(", ", " ", toString(variables))
    prepend <- paste0("%MACRO est_ispatch;\n",
                      "\t/* %LISTE (&cou..swn,var,%STR(sou='CRON'),); %PUT &list_var; */\n",
                      "\t/***********************************************************/\n",
                      "\t%LET varCRON=", list.var, ";\n",
                      "\t/***********************************************************/\n",
                      "\t%DO f=1 %TO %NWORD(&varCRON);\n",
                      "\t%LET variable=%SCAN(&varCRON,&f,\" \");\n")
    append <- paste0("\t\t\t%eAGG2 (&variable,NAPATCH);\n",
                     "\t\t\t%eSavePATCH (&variable); %eSaveDATE (&variable); %eDelTABLE (&cou..&variable);\n",
                     "\t%END;\n",
                     "\n%MEND est_ispatch;\n",
                     "%est_ispatch;\n")
    list.sou <- sources[[cou]]
    list.sou <- sub("NSO", "NSONA", list.sou)
    list.sou <- c(list.sou, "UNSDSNA")
    main <- "NSONA"
    mainflag <- "*NSO"
    if ("STD-SNA"%in%list.sou)
    {
        list.sou <- sub("STD-SNA", "ANA", list.sou)
        main <- "ANA"
        mainflag <- "*ANA"
    }
    ## if (cou%in%STAN.COUEU)
    if (cou%in%STAN.COU[["EU"]]) {
        list.sou <- sub("NSONA", "NAMA", list.sou)
        main <- "NAMA"
        mainflag <- "*ANA"
    }

    list.sou.string <- gsub(", ", "-", toString(list.sou))

    lines.prog <- rbind.data.frame(
        c(TRUE, "START", paste0("\t\t%eSTART (&variable,", list.sou.string, ");")),
        c(FALSE, "ISPATCH", paste0("\t\t\t%eMAIN2 (&variable,ISPATCH,SSIS,SSIS,0);")),
        c(TRUE, "NA.MAIN", paste0("\n\t\t\t%eMAIN2 (&variable,NAPATCH,", main, ",", mainflag, ",0);")),
        c(FALSE, "ADD.ZERO", paste0("\t\t\t%eADD_row (NAPATCH,&variable,", add.zero, ",", period, ",0,", mainflag, ");\n",
                                    "\t\t\t%eADJUST4 (&variable,NAPATCH,", mainflag, ");")),
        c(FALSE, "ISPATCH", paste0("\t\t\t\t%eEXTEND6 (&variable,NAPATCH,ISPATCH,EIS3,1-2-3-4-5-6-7);\n",
                                   "\t\t\t\t%eDETAIL3 (&variable,NAPATCH,ISPATCH,EIS3,2-3-4-5-6-7);\n",
                                   "\t\t\t\t%eADJUST4 (&variable,NAPATCH,", mainflag, ");")),
        c(FALSE, "SPEC", paste0("\t\t\t\t\t%eEXTEND6 (&variable,NAPATCH,SPEC,ESUT,1-2-3-4-5-6-7);\n",
                                "\t\t\t\t\t%eDETAIL3 (&variable,NAPATCH,SPEC,ESUT,2-3-4-5-6-7);\n",
                                "\t\t\t\t\t%eADJUST4 (&variable,NAPATCH,", mainflag, ");")),
        c(FALSE, "SUT", paste0("\t\t\t\t\t\t%eEXTEND6 (&variable,NAPATCH,SUT,ESUT,1-2-3-4-5-6-7);\n",
                               "\t\t\t\t\t\t%eDETAIL3 (&variable,NAPATCH,SUT,ESUT,2-3-4-5-6-7);\n",
                               "\t\t\t\t\t\t%eADJUST4 (&variable,NAPATCH,", mainflag, ");")),
        c(FALSE, "SUT2", paste0("\t\t\t\t\t\t%eEXTEND6 (&variable,NAPATCH,SUT2,ESUT,1-2-3-4-5-6-7);\n",
                               "\t\t\t\t\t\t%eDETAIL3 (&variable,NAPATCH,SUT2,ESUT,2-3-4-5-6-7);\n",
                               "\t\t\t\t\t\t%eADJUST4 (&variable,NAPATCH,", mainflag, ");")),
        c(FALSE, "SUT3", paste0("\t\t\t\t\t\t%eEXTEND6 (&variable,NAPATCH,SUT3,ESUT,1-2-3-4-5-6-7);\n",
                               "\t\t\t\t\t\t%eDETAIL3 (&variable,NAPATCH,SUT3,ESUT,2-3-4-5-6-7);\n",
                               "\t\t\t\t\t\t%eADJUST4 (&variable,NAPATCH,", mainflag, ");")),
        c(FALSE, "AAA2", paste0("\t\t\t\t\t\t%eEXTEND6 (&variable,NAPATCH,AAA2,ESUT,1-2-3-4-5-6-7);\n",
                               "\t\t\t\t\t\t%eDETAIL3 (&variable,NAPATCH,AAA2,ESUT,2-3-4-5-6-7);\n",
                               "\t\t\t\t\t\t%eADJUST4 (&variable,NAPATCH,", mainflag, ");")),
        c(FALSE, "AAA3", paste0("\t\t\t\t\t\t%eEXTEND6 (&variable,NAPATCH,AAA3,ESUT,1-2-3-4-5-6-7);\n",
                               "\t\t\t\t\t\t%eDETAIL3 (&variable,NAPATCH,AAA3,ESUT,2-3-4-5-6-7);\n",
                               "\t\t\t\t\t\t%eADJUST4 (&variable,NAPATCH,", mainflag, ");")),
        c(FALSE, "ALFS", paste0("\t\t\t\t\t\t%eEXTEND6 (&variable,NAPATCH,ALFS,ELFS,1-2-3-4-5-6-7);\n",
                               "\t\t\t\t\t\t%eDETAIL3 (&variable,NAPATCH,ALFS,ELFS,2-3-4-5-6-7);\n",
                               "\t\t\t\t\t\t%eADJUST4 (&variable,NAPATCH,", mainflag, ");")),
        c(TRUE, "UNSDSNA", paste0("\t\t\t\t\t\t\t%eEXTEND6 (&variable,NAPATCH,UNSDSNA,ENSO,1-2-3-4-5-6-7);\n",
                                  "\t\t\t\t\t\t\t%eDETAIL3 (&variable,NAPATCH,UNSDSNA,ENSO,2-3-4-5-6-7);\n",
                                  "\t\t\t\t\t\t\t%eADJUST4 (&variable,NAPATCH,", mainflag, ");")),
        c(FALSE, "DEL.ZERO", paste0("\t\t\t\t%eDEL_row (&cou..&variable,NAPATCH,,", add.zero, ",,);")))

    names(lines.prog) <- c("fixed", "line", "sascode")
    body <- lines.prog$sascode[lines.prog$fixed==TRUE | lines.prog$line%in%list.sou]
    text <- c(prepend, paste0(body), append)
    writeLines(text = text, con = fileCon)
    close(fileCon)
    if (quiet==FALSE) return(text)
}
