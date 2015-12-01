#' Create Diagnoal Converter
#'
#' Create Diagonal Matrix Aggregation Converters
#'
#' Create diagonal converters for the aggregation of matrices. Tested with two cascaded row dimensions and one column dimension.
#'
#' @param data .
#' @param toRow .
#' @param toCol .
#' @param displayConv .
#'
#' @author OECD STAN
#' @keywords IO
#' @seealso \code{\link{convShow}}
#' @export
#' @examples
#' convCreateDiag(dim=list(row=c(10,5),col=c(10)),agg.row1=c(1:10),agg.row2=c(1:2))

convCreateDiag <- function(dim=list(row=c(58,37),col=c(58)),
                           agg.row1=c(1:10),
                           agg.row2=c(1:18)) {

    vdim <- NULL
    for (d in seq(along=dim)) {
        vdim[d] <- 1
        for (i in seq(along=dim[[d]])) {
            vdim[d] <- vdim[d] * dim[[d]][i]
        }
    }
    ##
    conv <- array(0, dim = vdim, dimnames = dimnames)

    for (i in agg.row1) {
        conv[(agg.row1[i]-1) * dim[[1]][2] + agg.row2, as.numeric(i)] <- 1
    }

    return(conv)
}
