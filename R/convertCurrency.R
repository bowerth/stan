#' Convert Currencies
#'
#' Apply currency conversion to monetary variables
#'
#' This function converts monetary values from national currency to US dollars.
#'
#' @param data a data frame with dimensions \code{cou}, \code{var}, \code{year} and \code{value}.
#' @param datacur a data frame with dimension \code{cou}, \code{var} and \code{value}.
#'
#' @author OECD STAN
#' @keywords currency
#' @seealso \code{\link{packageData}}
#' @export
#' @examples
#' convertCurrency(data=DATA.EUNAMAR2, datacur=DATA.XRATES[DATA.XRATES$var=="EXCH",])

convertCurrency <- function(data=stop("'data' must be specified"),
                            datacur=DATA.XRATES[DATA.XRATES$var=="EXCH",],
                            tounit="USD")
    {
        data <- merge(data, datacur, by = c("cou", "year"))
        names(data) <-  sub("var.x", "var", names(data))
        names(data) <-  sub("value.x", "value", names(data))
        if (tounit=="USD") {
            data$value[data$var%in%STAN.VAR[["MON"]]] <- data$value[data$var%in%STAN.VAR[["MON"]]] / data$value.y[data$var%in%STAN.VAR[["MON"]]]
        } else if (tounit=="NCU") {
            data$value[data$var%in%STAN.VAR[["MON"]]] <- data$value[data$var%in%STAN.VAR[["MON"]]] * data$value.y[data$var%in%STAN.VAR[["MON"]]]
        }
        data <- data[, !colnames(data)%in%c("var.y", "value.y")]
        return(data)
    }

