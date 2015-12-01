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
#' convCreateDiag2(dim=list(row=c(10,5),col=c(10)),agg.row1=c(1:10),agg.row2=c(1:2))

convCreateDiag2 <- function(dim=list(row=c(62,34),col=c(34)),
                           agg.row1=c(1:62),
                           agg.row2=c(1:34)) {

    vdim <- NULL
    for (d in seq(along=dim)) {
        vdim[d] <- 1
        for (i in seq(along=dim[[d]])) {
            vdim[d] <- vdim[d] * dim[[d]][i]
        }
    }
    ##
    conv <- array(0, dim = vdim, dimnames = dimnames)

    for (i in agg.row2) {
        conv[agg.row1 * dim[[1]][2] - dim[[1]][2] + i, i] <- 1
    }

    ## convShow(conv)
    ## conv[1:10, 1:10]
    return(conv)
}
