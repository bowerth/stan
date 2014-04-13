#' Dim Merge
#'
#' Dimension Merge
#'
#' Self-join data frame on two members of same dimension.
#'
#' @param data a dataframe with "value" dimension and at lease one of "var" or "ind".
#' @param dim a character string specifying "var" or "ind".
#' @param dim1 a character string specifying the first dimension member (x).
#' @param dim2 a character string specifying the second dimension member (y).
#'
#' @author OECD STAN
#' @keywords transform
#' @seealso \code{\link{mergeCalc}}
#' @export
#' @examples
#' x <- data.frame("ind" = c(rep('D01T03', 2), rep('D05T09', 2)), "var" = rep(c('PROD', 'VALU'), 2), "value" = c(1:4))
#' x <- dimMerge(data=x, dim="var", dim1="PROD", dim2="VALU")
#' x <- dimMerge(data=x, dim="ind", dim1="D01T03", dim2="D05T09")

dimMerge <- function(data, dim, dim1, dim2)
{
    if (dim=="var")
    {
        data1 <- data[data$var==dim1, ]
        data2 <- data[data$var==dim2, ]
    }
    if (dim=="ind")
    {
        data1 <- data[data$ind%in%dim1, ]
        data2 <- data[data$ind==dim2, ]
    }
    data.merge <- merge(data1, data2, by = names(data)[!names(data)%in%c(dim, "value")])
    names(data.merge) <- sub(dim, "dim", names(data.merge))
    return(data.merge)
}
