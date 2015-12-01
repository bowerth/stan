#' Show Converter
#'
#' Show Matrix Aggregation Converters
#'
#' Show converters for the aggregation of matrices.
#'
#' @param m a matrix.
#' @param xLabels labels for x dimension.
#' @param yLabels labels for y dimension.
#'
#' @author OECD STAN
#' @keywords IO
#' @seealso \code{\link{convCreate}}
#' @export
#' @examples
#' see "convCreate.svg" in /vignettes/diagrams
#' m <- convCreate(dim=list(row=c(10,5),col=c(10)),agg.row1=c(1:10),agg.row2=c(1:2),agg.col1=c(5),horiz=FALSE,dimnames=NULL)
#' convShow(m)

convShow <- function(m,
                      xLabels=NULL,
                      yLabels=NULL) {
  z <- t(m)
  z <- z[, rev(seq_len(ncol(z)))] ## reverse column order - creates numeric from matrix
  if (is.null(xLabels)) xLabels <- 1:dim(z)[1]
  if (is.null(yLabels)) yLabels <- 1:dim(z)[2]
  i <- image(x = c(1:length(xLabels)),
             y = c(1:length(yLabels)),
             z = z,
             col = c("white", "green"),
             axes = FALSE)
  axis(BOTTOM<-1, at=1:length(xLabels), labels=xLabels, las = HORIZONTAL <- 1, cex.axis=0.7, tick=FALSE)
  axis(TOP<-3, at=1:length(xLabels), labels=xLabels, las = HORIZONTAL <- 1, cex.axis=0.7, tick=FALSE)
  axis(LEFT<-2, at=1:length(yLabels), labels=rev(yLabels), las = VERTICAL <- 2, cex.axis=0.7, tick=FALSE)
  axis(RIGHT<-4, at=1:length(yLabels), labels=rev(yLabels), las = VERTICAL <- 2, cex.axis=0.7, tick=FALSE)
  ## ?axis
  return(i)
}

