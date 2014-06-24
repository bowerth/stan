#' SAS load
#'
#' Create SAS loading programs
#'
#' Create or modify SAS loading programs for STAN based on a list of countries and sources.
#'
#' @param cou a 3-digit country ISO code character string.
#' @param isic an integer to specify the version of STAN.
#' @param sources a list naming the sources to load.
#' @param delete a logical to delete data already existing in SWN.
#' @param load.swn a logical to re-load data from SAS STAN libary into SWN.
#' @param period a character string with the update period in file names.
#' @param quiet a logical to return the created SAS program contents.
#'
#' @author OECD STAN
#' @keywords SAS
#' @seealso \code{\link{SASbatch}}, \code{\link{SASestim}}, \code{\link{SASpub}}
#' @export
#' @examples
#' SASload(cou="BGR", sources=list(BGR = c('NSONA', 'SUT')))

SASload <- function(cou=stop("'cou' must be specified"),
                    isic=4,
                    sources=list(BGR = c('NSO', 'SUT', 'STD-SNA')),
                    delete=FALSE,
                    load.swn=TRUE,
                    period="2013-14",
                    quiet=FALSE)
{
    if (isic==3) path.sas <- PATH.SASi3
    if (isic==4) path.sas <- PATH.SASi4
    ##
    prog.cou <- paste0(path.sas, "progs\\PAYS\\", cou, "\\", cou, "_load.sas")
    fileCon <- file(prog.cou)
    if (!file.exists(prog.cou)) file.create(prog.cou)
    if (load.swn==TRUE) load.swn.str <- "oui" else load.swn.str <- "non"
    lines.prog <- rbind.data.frame(
        c(FALSE, "DELETE", "PROC SQL; DELETE FROM &cou..swn; QUIT;"),
        c(TRUE, "SWN", paste0("%LoadSWN (", load.swn.str, ");")),
        c(TRUE, "NATIONAL", "/* national sources */"),
        c(FALSE, "NSO", paste0("%LoadXLS (R_", cou, "_NSO_4SAS_", period, "); /* NSONA */")),
        c(FALSE, "STD-SNA", paste0("%LoadXLS (R_ANA_4SAS_", period, "); /* STD-SNA */")),
        c(FALSE, "SPEC", paste0("%LoadXLS (R_", cou, "_SPEC_4SAS_", period, "); /* SPEC */")),
        c(FALSE, "SUT", paste0("%LoadXLS (R_", cou, "_SUT_4SAS_", period, "); /* SUT */")),
        c(FALSE, "SUT2", paste0("%LoadXLS (R_", cou, "_SUT2_4SAS_", period, "); /* SUT2 */")),
        c(FALSE, "SUT3", paste0("%LoadXLS (R_", cou, "_SUT3_4SAS_", period, "); /* SUT3 */")),
        c(FALSE, "AAA2", paste0("%LoadXLS (R_", cou, "_AAA2_4SAS_", period, "); /* AAA2 */")),
        c(FALSE, "AAA3", paste0("%LoadXLS (R_", cou, "_AAA3_4SAS_", period, "); /* AAA3 */")),
        c(FALSE, "ALFS", paste0("%LoadXLS (R_", cou, "_AAA3_4SAS_", period, "); /* AAA3 */")),
        ## better load harmonized sources within SAS
        c(TRUE, "HARMONIZED", "/* harmonized sources */"),
        c(FALSE, "UNIDO", paste0("%LoadXLS (", cou, "_UNIDO_4SAS_", period, "); /* CRON */")),
        c(FALSE, "WIOD", paste0("%LoadXLS (", cou, "_WIOD_4SAS_", period, "); /* WIOD */")))
    names(lines.prog) <- c("fixed", "line", "sascode")
    if (delete==TRUE) list.sou <- c("DELETE", sources[[cou]]) else list.sou <- sources[[cou]]
    text <- lines.prog$sascode[lines.prog$fixed==TRUE | lines.prog$line%in%list.sou]
    text <- paste(text)
    writeLines(text = text, con = fileCon)
    close(fileCon)
    if (quiet==FALSE) return(text)
}
