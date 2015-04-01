#' Calc formula
#'
#' Apply industry conversion based on calculation formula
#'
#' Uses a data frame containing formulas to convert industry classification.
#'
#' @param data data in wide format with industries in columns.
#' @param formula data frame with "ind" and "formula" column.
#' @param id.vars character vector to specify preserved columns.
#'
#' @author OECD STAN
#' @keywords SAS
#' @seealso \code{\link{SASformula}}
#' @export
#' @examples
#' require(reshape2)
#' require(stanData)
#' data(STANNAi4)
#' cou <- "AUT"
#' var <- "VALU"
#' data <- DATA.STAN
#' data <- data[data$cou%in%cou &
#'              data$var%in%var,]
#' data <- dcast(data, cou + var + year ~ ind, value.var = "value")
#' file <- paste0(PATH.SASi4, "Lists\\MDL_STAN_i4_i3_A64.txt")
#' line.remove <- c("/* aggregate */")
#' formula <- SASformula(file=file, line.remove=line.remove)
#' data <- calcFormula(data=data, formula=formula, id.vars=id.vars)
#' data <- melt(data, id.vars = id.vars, variable.name = "ind", na.rm = TRUE)
#' data <- subset(data, select = c("cou", "var", "ind", "year", "value"))

calcFormula <- function(data=stop("'data' must be specified"),
                        formula=stop("'formula' must be specified"),
                        id.vars=c("cou", "var", "year")
                        )
{
    attach(data)
    for (ind in formula$ind) {
      ## if (formula$formula[formula$ind==ind]=="NA") {
      ##   try(eval(parse(text = paste0('data$', ind, ' <- NA'))), silent = TRUE)
      ## } else {
        try(eval(parse(text = paste0('data$', ind, ' <- ', formula$formula[formula$ind==ind]))), silent = TRUE)
      ## }
    }
    detach(data)
    data <- data[,colnames(data)%in%union(id.vars, formula$ind)]
    return(data)
}
