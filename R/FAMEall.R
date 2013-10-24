#' FAME all
#'
#' Combine FAME functions
#'
#' Call FAMEprepare, FAMEload and FAMEtransform together.
#'
#' @param cou a 3-digit country ISO code character string.
#' @param year an integer specifying the year of interest.
#' @param isic a character sring matching the industry classification used in the dataset name.
#' @param tables an integer vector specifying the SNA tables to be looked for.
#' @param version optional: the version number of the file to be used.
#'
#' @author OECD STAN
#' @keywords FAME
#' @seealso \code{\link{FAMEprepare}}, \code{\link{FAMEload}}, \code{\link{FAMEtransform}}
#' @export
#' @examples
#' FAMEall(cou="ITA")

FAMEall <- function(cou=stop("'cou' must be specified"),
                    isic=4,
                    classification='NACE2',
                    year=2012,
                    tables=c(301,302,303,2000),
                    version='latest',
                    ...)
{
    FAMEprepare(cou=cou,
                isic=isic,
                classification=classification,
                year=year,
                tables=tables,
                version=version)
    FAMEload(cou=cou)
    FAMEtransform(cou=cou)
}
