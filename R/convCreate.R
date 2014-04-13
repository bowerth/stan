#' Create Converter
#'
#' Create Matrix Aggregation Converters
#'
#' Create converters for the aggregation of matrices. Tested with two cascaded row dimensions and one column dimension.
#'
#' @param dim a list with two non-empty elements specifying the dimensions of the converter.
#' @param aggdim1 a numeric vector with two elements specifying the first aggregation dimension.
#' @param aggrng1 a numeric vector with two elements specifying the aggregation range selected by \code{aggdim1}.
#' @param aggdim2 a numeric vector with two elements specifying the second aggregation dimension.
#' @param aggrng2 a numeric vector with two elements specifying the aggregation range selected by \code{aggdim2}.
#' @param fixed a numeric vector specifying the members to aggregate in dimension other than \code{aggdim1}.
#' @param horiz logical specifying if the converter shall be applied horizontally.
#' @param dimnames a list with as many items as dimensions, each item as many elements as dimension members.
#'
#' @author OECD STAN
#' @keywords IO
#' @seealso \code{\link{indAggregate}}
#' @export
#' @examples
#' convCreate(dim=list(row=c(10,5),col=c(10)),agg.row1=c(1:10),agg.row2=c(1:2),agg.col1=c(5),horiz=FALSE,dimnames=NULL)

convCreate <- function(dim=list(row=c(58,37),col=c(58)),
                       agg.row1=c(1:10),
                       agg.row2=c(1:2),
                       agg.col1=c(5),
                       horiz=FALSE,
                       dimnames=NULL)
{

    vdim <- NULL
    for (d in seq(along=dim))
    {
        vdim[d] <- 1
        for (i in seq(along=dim[[d]]))
        {
            vdim[d] <- vdim[d] * dim[[d]][i]
        }
    }
    ##
    conv <- array(0, dim = vdim, dimnames = dimnames)

    ## if (horiz==FALSE)
    ## {
    ##     for (i in c(1:dim[[aggdim1[1]]][aggdim1[2] - 1]))
    ##     {
    ##         row0 <- (i - 1) * dim[[aggdim1[1]]][aggdim1[2]]
    ##         conv[c((row0 + aggrng1[1]):(row0 + aggrng1[2])), fixed] <- 1
    ##     }
    ## } else
    ## {
    ##     for (j in seq(along=fixed))
    ##     {
    ##         for (i in c(1:dim[[aggdim1[1]]][aggdim1[2] - 1]))
    ##         {
    ##             row0 <- (fixed[j] - 1) * dim[[aggdim1[1]]][aggdim1[2]]
    ##             conv[c((row0 + aggrng1[1]):(row0 + aggrng1[2])), i] <- 1
    ##         }
    ##     }
    ## }

    for (i in agg.row1)
    {
        for (ii in agg.row2)
        {
            for (j in agg.col1)
            {
                if (horiz==FALSE)
                {
                    conv[(i-1) * dim[[1]][2] + ii, j] <- 1
                } else
                {
                    conv[(j-1) * dim[[1]][2] + ii, i] <- 1
                }
            }
        }
    }

    return(conv)
}

## dim=list(row=c(58,37),col=c(58))
## agg.row1 = agg.couIM
## agg.row2 = agg.indFD
## agg.col1 = agg.couFD
## View(conv)
## View(conv2)
