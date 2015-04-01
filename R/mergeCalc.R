#' mergeCalc
#'
#' Calculates ratio or difference
#'
#' Calculate new variable from data frame merged with \code{\link{dimMerge}}.
#'
#' @param data a dataframe with "value.x" and "value.y" dimensions.
#' @param diff logical to calculate difference, default is ratio.
#'
#' @author OECD STAN
#' @keywords calculate
#' @seealso \code{\link{dimMerge}}
#' @export
#' @examples
#' x <- data.frame("ind" = c(rep('D01T03', 2), rep('D05T09', 2)), "var" = rep(c('PROD', 'VALU'), 2), "value" = c(1:4))
#' x <- dimMerge(data=x, dim="var", dim1="PROD", dim2="VALU")
#' x <- mergeCalc(data=x, diff=FALSE)

mergeCalc <- function(data, diff = diff)
{
    if (diff==TRUE)
    {
        data$value <- data$value.x - data$value.y
        data$calc <- paste0(data$dim.x, "diff", data$dim.y)
    } else {
        data$value <- data$value.x / data$value.y
        data$calc <- paste0(data$dim.x, "share", data$dim.y)
    }
    return(data)
}
