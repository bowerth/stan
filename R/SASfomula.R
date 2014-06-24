#' SAS formula
#'
#' Convert SAS program for R
#'
#' Convert a SAS program with DATA step calculation formulas to an R data frame
#'
#' @param file filepath to SAS program.
#' @param line.remove character vector with rows to remove.
#'
#' @author OECD STAN
#' @keywords SAS
#' @seealso \code{\link{calcFormula}}
#' @export
#' @examples
#' file <- paste0(PATH.SASi4, "Lists\\MDL_STAN_i4_i3_A64.txt")
#' line.remove <- c("/* aggregate */")
#' SASformula(file=file, line.remove=line.remove)

SASformula <- function(file=stop("'file' must be specified"),
                       line.remove=NULL
                       )
{
    formula <- readLines(file)
    if (length(line.remove) > 0) formula <- formula[!formula%in%line.remove]
    formula <- sub(";", "", formula)
    formula <- gsub(" ", "", formula)
    formula <- formula[!formula==""]
    X <- strsplit(formula, split = "=")
    df.formula <- data.frame(ind = sapply(X, "[[", 1),
                             formula = sapply(X, "[[", 2))
    return(df.formula)
}
