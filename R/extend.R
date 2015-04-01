#' extend
#'
#' Extend time series
#'
#' Extend time series using growth rates of secondary sources
#'
#' @param data a dataframe object.
#' @param namesou a character vector specifying the main source and secondary source(s).
#'
#' @author OECD STAN
#' @keywords estimate
#' @seealso \code{\link{detail}}
#' @export
#' @examples
#' data table: cou, var, ind, year, value
#' extend(data=data.ext.start, namesou=namesou.ext)

## namesou <- c("STANandBTDi4", "STANandBTD")
## namesou <- c("STANandBTD", "STANandBTDi4")
## data.main <- subset(data.main, year%in%c(2003:2012))

## data <- read.csv("/home/OECDMAIN/werth_admin/Downloads/stani4Estimate.csv")
## data <- read.csv("/home/stan/Downloads/stani4Estimate.csv")
## data <- read.csv("/home/stan/Downloads/stani4Estimate.csv")
## namesou <- c("NSO", "STANi4")
## require(dplyr)
## extend(data, namesou)

extend <- function(data, namesou) {

  data.main <- data[data$sou==namesou[1],]

  if (nrow(data.main) > 0) {

        time.main <- c(min(data.main$year):max(data.main$year))
        data.main <- base::merge(data.main, data.frame(year = time.main), by = "year", all = TRUE) # insert NAs for missing years
        data.main.ts <- stats::ts(data.main$value, start = min(data.main$year), end = max(data.main$year))
        for (sou in 2:length(namesou)) {
            ## sou <- 2
            data.ext <- data[data$sou==namesou[sou],]
            if (nrow(data.ext) > 0) {
                time.ext <- c(min(data.ext$year):max(data.ext$year))
                data.ext <- base::merge(data.ext, data.frame(year = time.ext), by = "year", all = TRUE) # insert NAs for missing years
                data.ext.ts <- stats::ts(data.ext$value, start = min(data.ext$year), end = max(data.ext$year)) # ?ts
                data.ext.ts.lag <- stats::ts(data.ext$value, start = min(data.ext$year)-1, end = max(data.ext$year)-1) # ?ts
                ## data.ext.ts.lag <- stats::lag(data.ext.ts)
                ## lag function modified by dplyr
                growth.ext.ts <- (data.ext.ts / data.ext.ts.lag)
                data.combine.ts <- stats::ts.union(data.main.ts, growth.ext.ts, data.ext.ts)[,-3]
                data.combine <- as.data.frame(data.combine.ts)
                names(data.combine) <- c("value", "rate")
                data.combine$year <- stats::time(data.combine.ts)
                for (dir in c("bw", "fw")) {# bw: backward extension, fw: forward extension
                    ## dir <- "bw"
                    ## dir <- "fw"
                    ##
                    if (dir=="bw") period.ext <- c(max(stats::time(data.main.ts)):min(stats::time(growth.ext.ts)))[-1]
                    ## ###############
                    ## error tracking
                    ## ###############
                    if (dir=="fw") period.ext <- c(min(stats::time(data.main.ts)):(max(stats::time(growth.ext.ts))+1))[-1]
                    ## ## fix: wrong growth rates because of namespace issues??
                    ## if (dir=="fw") period.ext <- c(min(stats::time(data.main.ts)):max(stats::time(growth.ext.ts)))[-1] # '+1' adds year that is not available
                    ## ###############
                    ## end tracking
                    ## ###############
                    for (yr in period.ext) {
                        if (is.na(data.combine$value[data.combine$year==yr])) {
                            if (dir=="bw") value <- data.combine$value[data.combine$year==yr+1] * data.combine$rate[data.combine$year==yr]
                            if (dir=="fw") value <- data.combine$value[data.combine$year==yr-1] / data.combine$rate[data.combine$year==yr-1]
                            ## ## fix: wrong growth rates because of namespace issues??
                            ## if (dir=="bw") value <- data.combine$value[data.combine$year==yr+1] / data.combine$rate[data.combine$year==yr]
                            ## if (dir=="fw") value <- data.combine$value[data.combine$year==yr-1] * data.combine$rate[data.combine$year==yr]
                            if (length(value) > 0) data.combine$value[data.combine$year==yr] <- value
                        }
                    }
                    data.main <- data.combine[!is.nan(data.combine$value) & !is.na(data.combine$value),]
                    data.main.ts <- stats::ts(data.main$value, start = min(data.main$year))
                }
                data.main <- data.combine[!is.nan(data.combine$value) & !is.na(data.combine$value),]
                data.main.ts <- stats::ts(data.main$value, start = min(data.main$year))
            }
        }

        return(data.main[,!colnames(data.main)=="rate"])
    }

}
