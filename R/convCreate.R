#' Create Converter
#'
#' Create Matrix Aggregation Converters
#'
#' Create converters for the aggregation of matrices. Tested with two cascaded row dimensions and one column dimension.
#'
#' @param dim a list with two non-empty elements specifying the dimensions of the converter.
#' @param agg.row1 a numeric vector with two elements specifying the first row aggregation dimension.
#' @param agg.row2 a numeric vector with two elements specifying the second row aggregation dimension.
#' @param agg.col1 a numeric vector with two elements specifying the column aggregation dimension.
#' @param horiz logical specifying if the converter shall be applied horizontally.
#' @param dimnames a list with as many items as dimensions, each item as many elements as dimension members.
#'
#' @author OECD STAN
#' @keywords IO
#' @seealso \code{\link{convShow}}
#' @export
#' @examples
#' see "convCreate.svg" in /vignettes/diagrams
#' convCreate(dim=list(row=c(10,5),col=c(10)),agg.row1=c(1:10),agg.row2=c(1:2),agg.col1=c(5),horiz=FALSE,dimnames=NULL)

convCreate <- function(dim=list(row=c(58,37),col=c(58)),
                       agg.row1=c(1:10),
                       agg.row2=c(1:2),
                       agg.col1=c(5),
                       horiz=FALSE,
                       dimnames=NULL)
{

    vdim <- NULL
    for (d in seq(along=dim)) {
        vdim[d] <- 1
        for (i in seq(along=dim[[d]])) {
            vdim[d] <- vdim[d] * dim[[d]][i]
        }
    }
    conv <- array(0, dim = vdim, dimnames = dimnames)

    for (i in agg.row1) {
        for (ii in agg.row2) {
            for (j in agg.col1) {
                if (horiz==FALSE) {
                    conv[(i-1) * dim[[1]][2] + ii, j] <- 1
                } else {
                    conv[(j-1) * dim[[1]][2] + ii, i] <- 1
                }
            }
        }
    }

    return(conv)
}
