#' estimate
#'
#' Create STAN estimates
#'
#' Combine extend and detail functions to time series according to supplied list of sources
#'
#' @param data a dataframe object
#' @param sources a data frame with main source and secondary sources for each combination in the data
#' @param period time period for estimation
#'
#' @author OECD STAN
#' @keywords estimate
#' @seealso \code{\link{extend}}, \code{\link{detail}}
#' @export
#' @examples
#' data table: cou, var, ind, year, value
#' sources table: cou, var, ind, est, MAIN, SEC
#' estimate(data=data.all, sources=est.array.d[est.array.d$est=="DET",], period=c(1995:2010))

estimate <- function(data=data.all,
                     sources=est.array.d[est.array.d$est=="DET",],
                     period=c(1995:2010))
{
    data.est <- NULL
    for (i in 1:nrow(sources)) {
        ## print(sources[i,])
        ##
        ## cat(paste0(toString(sapply(sources[i,], function (x) as.character(x))), '\n'))
        ##
        sou.main = sources$MAIN[i]
        sou.sec = strsplit(sources$SEC[i], split = ", ")[[1]]
        cou = as.character(sources$cou[i])
        var = as.character(sources$var[i])
        ind = as.character(sources$ind[i])
        est = as.character(sources$est[i])
        ##
        namesou <- c(sou.main, sou.sec)
        ##
        if (length(namesou) >= 2) {
            ##
            if (est=="EXT") {
                ##
                data.patch.ext <- NULL
                data.ext.start <- data[data$cou==cou &
                                       data$sou%in%namesou &
                                       data$var==var &
                                       data$ind==ind &
                                       data$year%in%period,]
                ##
                if (nrow(data.ext.start[data.ext.start$sou%in%sou.sec,]) >= 2) { # need more than one consecutive year
                    if (ind%in%data.ext.start$ind[data.ext.start$sou==sou.main] &
                        ind%in%data.ext.start$ind[data.ext.start$sou%in%sou.sec]) {
                        ##
                        PATCHEXT <- stan::extend(data = data.ext.start, namesou = c(sou.main, sou.sec))
                        ##
                        if (nrow(PATCHEXT) > 0) {
                            data.patch.ext <- data.frame(cou = cou,
                                                         sou = "PATCHEXT",
                                                         var = var,
                                                         year = PATCHEXT$year,
                                                         ind = ind,
                                                         value = PATCHEXT$value)
                        }
                        data.est <- rbind(data.est, data.patch.ext)
                    }
                }
            } # end if(est=="EXT") { ... }
            if (est=="DET") {
                ##
                data.patch.det <- NULL
                newsou <- "PATCHDET"
                ind.peers <- ind
                ind.parent <- as.character(STANi3.HIERARCHYINV[[ind]])
                data.det.start <- data[data$cou==cou &
                                       data$sou%in%namesou &
                                       data$var==var &
                                       data$ind%in%c(ind.parent[1], ind.peers) &
                                       data$year%in%period &
                                       data$value!=0,]
                ## possibly introducing breaks!!
                data.det.start <- rbind(data.det.start, data.est[data.est$ind%in%ind.parent[1],])
                data.det.start$sou <- sub("PATCHDET", "PATCHEXT", data.det.start$sou)
                data.det.start <- data.det.start[!duplicated(data.det.start[,!colnames(data.det.start)%in%c("value")]),]
                if (nrow(data.det.start[data.det.start$sou==sou.main,]) > 0 &
                    ind%in%data.det.start$ind[data.det.start$sou==sou.sec]) {
                    ## if parent industry in main source: calculate using this number
                    if (ind.parent[1]%in%unique(data.det.start$ind[data.det.start$sou==sou.main])) {
                        PATCHDET <- try(stan::detail(data = data.det.start,
                                                     namesou = namesou,
                                                     ind.parent = ind.parent[1],
                                                     ind.peers = ind.peers),
                                        silent = TRUE)
                    } else { # if parent industry not in main source, look for first parent
                        p <- 2
                        parent.in.namesou <- FALSE
                        while (p==2 | (p <= length(ind.parent) & parent.in.namesou==FALSE)) {
                            data.det.start.p <- data[data$cou==cou &
                                                     data$sou%in%namesou &
                                                     data$var==var &
                                                     data$ind%in%ind.parent[1:p] &
                                                     data$year%in%period,]
                            ##
                            if (ind.parent[p]%in%unique(data.det.start.p$ind[data.det.start.p$sou==sou.main])) {
                                PATCHDET.p <- stan::detail(data = data.det.start.p,
                                                           namesou = namesou,
                                                           ind.parent = ind.parent[p],
                                                           ind.peers = ind.parent)
                                ##
                                data.patch.det.p <- data.frame(cou = cou,
                                                               sou = newsou,
                                                               var = var,
                                                               year = PATCHDET.p$year,
                                                               ind = PATCHDET.p$ind,
                                                               value = PATCHDET.p$value)
                            }
                            if (exists("data.patch.det.p")==TRUE) {
                                parent.in.namesou <- ind.parent[p]%in%unique(data.patch.det.p$ind[data.patch.det.p$sou=="PATCHDET"])
                            } else {
                                parent.in.namesou <- FALSE
                            }
                            p = p + 1
                        }
                        if (nrow(data.patch.det.p) > 0) {
                            PATCHDET <- stan::detail(data = rbind(data.det.start, data.patch.det.p),
                                                     namesou = sub(sou.main, newsou, namesou),
                                                     ind.parent = ind.parent[1],
                                                     ind.peers = ind.peers)
                        }
                    }
                    if (!is.null(nrow(PATCHDET))) {
                        if (nrow(PATCHDET) > 0) {
                            data.patch.det <- data.frame(cou = cou,
                                                         sou = newsou,
                                                         var = var,
                                                         year = PATCHDET$year,
                                                         ind = PATCHDET$ind,
                                                         value = PATCHDET$value)
                        }
                    }
                }
                data.patch.det <- data.patch.det[data.patch.det$ind==ind,]
                data.patch.det <- data.patch.det[!is.infinite(data.patch.det$value) & data.patch.det$value!=0,]
                data.est <- rbind(data.est, data.patch.det)
            } # end if(est=="DET") { ... }
        }
    }
    return(data.est)
}

