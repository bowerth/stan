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

extend <- function(data, namesou)
{
    data.main <- data[data$sou==namesou[1],]
    if (nrow(data.main) > 0) {
        time.main <- c(min(data.main$year):max(data.main$year))
        data.main <- merge(data.main, data.frame(year = time.main), by = "year", all = TRUE)
        data.main.ts <- ts(data.main$value, start = min(data.main$year))
        for (sou in 2:length(namesou))
        {
            ## sou <- 2
            data.ext <- data[data$sou==namesou[sou],]
            if (nrow(data.ext) > 0) {
                time.ext <- c(min(data.ext$year):max(data.ext$year))
                data.ext <- merge(data.ext, data.frame(year = time.ext), by = "year", all = TRUE)
                data.ext.ts <- ts(data.ext$value, start = min(data.ext$year))
                for (dir in c("bw", "fw")) # bw: backward extension, fw: forward extension
                {
                    ## dir <- "bw"
                    data.ext.ts.lag <- lag(data.ext.ts)
                    growth.ext.ts <- (data.ext.ts / data.ext.ts.lag)
                    data.combine.ts <- ts.union(data.main.ts, growth.ext.ts, data.ext.ts)[,-3]
                    data.combine <- as.data.frame(data.combine.ts)
                    names(data.combine) <- c("value", "rate")
                    data.combine$year <- time(data.combine.ts)
                    ##
                    if (dir=="bw") period.ext <- c(max(time(data.main.ts)):min(time(growth.ext.ts)))[-1]
                    if (dir=="fw") period.ext <- c(min(time(data.main.ts)):(max(time(growth.ext.ts))+1))[-1]
                    for (yr in period.ext)
                    {
                        if (is.na(data.combine$value[data.combine$year==yr]))
                        {
                            if (dir=="bw") value <- data.combine$value[data.combine$year==yr+1] * data.combine$rate[data.combine$year==yr]
                            if (dir=="fw") value <- data.combine$value[data.combine$year==yr-1] / data.combine$rate[data.combine$year==yr-1]
                            if (length(value) > 0) data.combine$value[data.combine$year==yr] <- value
                        }
                    }
                    data.main <- data.combine[!is.nan(data.combine$value) & !is.na(data.combine$value),]
                    data.main.ts <- ts(data.main$value, start = min(data.main$year))
                }

                data.main <- data.combine[!is.nan(data.combine$value) & !is.na(data.combine$value),]
                data.main.ts <- ts(data.main$value, start = min(data.main$year))
            }
        }
    return(data.main[,!colnames(data.main)=="rate"])
    }
}


# data.ext.start <- rbind.data.frame(
#     c("AUT", "STANandBTD", "VALU", 1994, "CTOTAL", 179170),
#     c("AUT", "STANandBTD", "VALU", 1995, "CTOTAL", 214550),
#     c("AUT", "STANandBTD", "VALU", 1996, "CTOTAL", 209518),
#     c("AUT", "STANandBTD", "VALU", 1997, "CTOTAL", 185876),
#     c("AUT", "STANandBTD", "VALU", 1998, "CTOTAL", 190802),
#     c("AUT", "STANandBTD", "VALU", 1999, "CTOTAL", 189093),
#     c("AUT", "STANandBTD", "VALU", 2000, "CTOTAL", 171906),
#     c("AUT", "STANandBTD", "VALU", 2001, "CTOTAL", 171845),
#     c("AUT", "STANandBTD", "VALU", 2002, "CTOTAL", 186051),
#     c("AUT", "STANandBTD", "VALU", 2003, "CTOTAL", 228002),
#     c("AUT", "STANandBTD", "VALU", 2004, "CTOTAL", 260689),
#     c("AUT", "STANandBTD", "VALU", 2005, "CTOTAL", 272865),
#     c("AUT", "STANandBTD", "VALU", 2006, "CTOTAL", 291676),
#     c("AUT", "STANandBTD", "VALU", 2007, "CTOTAL", 336840),
#     c("AUT", "STANandBTD", "VALU", 2008, "CTOTAL", 375777),
#     c("AUT", "STANandBTD", "VALU", 2009, "CTOTAL", 344515),
#     c("AUT", "STANandBTDi4", "VALU", 1994, "CTOTAL", 179421),
#     c("AUT", "STANandBTDi4", "VALU", 1995, "CTOTAL", 214796),
#     c("AUT", "STANandBTDi4", "VALU", 1996, "CTOTAL", 210053),
#     c("AUT", "STANandBTDi4", "VALU", 1997, "CTOTAL", 186825),
#     c("AUT", "STANandBTDi4", "VALU", 1998, "CTOTAL", 191981),
#     c("AUT", "STANandBTDi4", "VALU", 1999, "CTOTAL", 190467),
#     c("AUT", "STANandBTDi4", "VALU", 2000, "CTOTAL", 172775),
#     c("AUT", "STANandBTDi4", "VALU", 2001, "CTOTAL", 173364),
#     c("AUT", "STANandBTDi4", "VALU", 2002, "CTOTAL", 187630),
#     c("AUT", "STANandBTDi4", "VALU", 2003, "CTOTAL", 229908),
#     c("AUT", "STANandBTDi4", "VALU", 2004, "CTOTAL", 263047),
#     c("AUT", "STANandBTDi4", "VALU", 2005, "CTOTAL", 274926),
#     c("AUT", "STANandBTDi4", "VALU", 2006, "CTOTAL", 294291),
#     c("AUT", "STANandBTDi4", "VALU", 2007, "CTOTAL", 339591),
#     c("AUT", "STANandBTDi4", "VALU", 2008, "CTOTAL", 375278),
#     c("AUT", "STANandBTDi4", "VALU", 2009, "CTOTAL", 346872),
#     c("AUT", "STANandBTDi4", "VALU", 2010, "CTOTAL", 341240),
#     c("AUT", "STANandBTDi4", "VALU", 2011, "CTOTAL", 376294)
#     )
# names(data.ext.start) <- c("cou", "sou", "var", "year", "ind", "value")

# data.ext.start$year <- as.numeric(as.character(data.ext.start$year))
# data.ext.start$value <- as.numeric(as.character(data.ext.start$value))

# namesou.ext <- c("STANandBTD", "STANandBTDi4")

# PATCHEXT <- stan::extend(data = data.ext.start, namesou = namesou.ext)

# function (data, namesou)
# {
#     data.main <- data[data$sou == namesou[1], ]
#     time.main <- c(min(data.main$year):max(data.main$year))
#     data.main <- merge(data.main, data.frame(year = time.main),
#         by = "year", all = TRUE)
#     data.main.ts <- ts(data.main$value, start = min(data.main$year))
#     for (sou in 2:length(namesou)) {
#         data.ext <- data[data$sou == namesou[sou], ]
#         time.ext <- c(min(data.ext$year):max(data.ext$year))
#         data.ext <- merge(data.ext, data.frame(year = time.ext),
#             by = "year", all = TRUE)
#         data.ext.ts <- ts(data.ext$value, start = min(data.ext$year))
#         for (dir in c("bw", "fw")) {
#             data.ext.ts.lag <- lag(data.ext.ts)
#             growth.ext.ts <- (data.ext.ts/data.ext.ts.lag)
#             data.combine.ts <- ts.union(data.main.ts, growth.ext.ts,
#                 data.ext.ts)[, -3]
#             data.combine <- as.data.frame(data.combine.ts)
#             names(data.combine) <- c("value", "rate")
#             data.combine$year <- time(data.combine.ts)
#             if (dir == "bw")
#                 period.ext <- c(max(time(data.main.ts)):min(time(growth.ext.ts)))[-1]
#             if (dir == "fw")
#                 period.ext <- c(min(time(data.main.ts)):(max(time(growth.ext.ts)) + 1))[-1]
#             for (yr in period.ext) {
#                 if (is.na(data.combine$value[data.combine$year == yr])) {
#                   if (dir == "bw")
#                     value <- data.combine$value[data.combine$year ==
#                       yr + 1] * data.combine$rate[data.combine$year ==
#                       yr]
#                   if (dir == "fw")
#                     value <- data.combine$value[data.combine$year ==
#                       yr - 1]/data.combine$rate[data.combine$year ==
#                       yr - 1]
#                   if (length(value) > 0)
#                     data.combine$value[data.combine$year == yr] <- value
#                 }
#             }
#             data.main <- data.combine[!is.nan(data.combine$value) &
#                 !is.na(data.combine$value), ]
#             data.main.ts <- ts(data.main$value, start = min(data.main$year))
#         }
#         data.main <- data.combine[!is.nan(data.combine$value) &
#             !is.na(data.combine$value), ]
#         data.main.ts <- ts(data.main$value, start = min(data.main$year))
#     }
#     return(data.main[, !colnames(data.main) == "rate"])
# }
