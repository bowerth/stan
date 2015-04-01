#' adjust
#'
#' Adjust time series
#'
#' Adjust time series using industry hierarchy
#'
#' @param data a dataframe object.
#' @param hierarchy the hierarchy for top-down adjustment applied to \code{variable.name}.
#' @param top highest aggregate in applied \code{hierarchy}
#'
#' @author OECD STAN
#' @keywords estimate
#' @seealso \code{\link{extend}}, \code{\link{detail}}
#' @export
#' @examples
#' data table: cou, var, ind, year, value
#' adjust(data=data.ext.start, variable.name="ind", value.var="value", hierarchy=STANi3.HIERARCHY, top="CTOTAL")

adjust <- function(data=stop("'data' must be specified"),
                   ## variable.name="ind",
                   ## value.var="value",
                   ## id.vars=NULLc("sou"),
                   hierarchy=STANi3.HIERARCHY,
                   top="CTOTAL"
                   )
{

    ## id.vars <- names(data)[!names(data)%in%c(variable.name, value.var)]
    ## id.vars <- id.vars[!id.vars%in%agg.vars]
    ## formula <- paste(gsub(", ", " + ", toString(id.vars)), "~", variable.name)
    ## data <- dcast(data, as.formula(formula), value.var = value.var)

    ## agg <- "D10T33"
    nameagg <- top
    while(length(nameagg) > 0) {
        parts.all <- NULL
        for (agg in nameagg) {
          
            ## if (agg%in%colnames(data)) {
              
                parts <- as.character(hierarchy[[agg]])
                if (length(parts) > 0) {
                    temp <- data[, colnames(data) %in% parts]
                    if (all(is.element(parts, colnames(temp))==TRUE &
                            agg%in%colnames(data))) {
                      sum.parts <- unname(apply(as.matrix(temp), 1, "sum"))
                      ratio <- cbind.data.frame(data[, colnames(data) %in% agg], sum.parts)
                      names(ratio) <- c(agg, "sum.parts")
                      ratio <- ratio[,agg] / ratio$sum
                      ## cat(paste0('Mean ratio: ', mean(ratio, na.rm = TRUE), '\n\n'))
                      temp <- cbind(temp, ratio)
                      for (col in names(temp)[names(temp)!="ratio"]) {
                        temp[, col] <- temp[, col] * temp[, "ratio"]
                      }
                      temp <- temp[,!colnames(temp)=="ratio"]
                      ##
                      data <- data[,!colnames(data)%in%names(temp)] # remove agg and its child industries from data
                      data <- cbind(data, temp)
                    } else {
                      ## remove parts columns from data
                      data <- data[, !colnames(data) %in% parts]
                    }
                    parts.all <- c(parts.all, parts)
                  }
              }
            
        ## }
        
        nameagg <- parts.all
    }

    ## View(data)
    ## data <- melt(data, id.vars = id.vars, variable.name = variable.name)

    return(data)
}
