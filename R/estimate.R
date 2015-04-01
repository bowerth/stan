#' estimate
#'
#' Create STAN estimates
#'
#' Combine extend and detail functions to time series according to supplied list of sources
#'
#' @param data a dataframe object
#' @param sources a data frame with main source and secondary sources for each combination in the data
#' @param period time period for estimation
#' @param isic industry classification
#'
#' @author OECD STAN
#' @keywords estimate
#' @seealso \code{\link{extend}}, \code{\link{detail}}
#' @export
#' @examples
#' data table: cou, var, ind, year, value
#' sources table: cou, var, ind, est, MAIN, SEC
#' data.all <- stani3Estimate.data.all # source stani3Estimate.R
#' exportcou <- c("NOR")
#' exportvar <- c("VALU", "PROD")
#' nameind <- as.character(ui.stani3Estimate.ind)
#' nameest <- c("DET", "EXT")
#' nameyear <- c(1995:2012)
#' est.array <- ui.stani3Estimate.est.array[exportcou, exportvar, nameind, nameest,,drop = FALSE]
#' est.array.m <- melt(est.array, id.vars = c("cou", "var", "ind", "est"), variable.name = "sou")
#' est.array.m$ind <- factor(est.array.m$ind, levels = STANi3.INDA60All)
#' est.array.d <- dcast(est.array.m, cou + var + ind + est ~ sou, value.var = "value")
#' x <- match(unique(est.array.m$ind), names(STANi3.HIERARCHYINV))
#' x <- x[!is.na(x)]
#' ind.parent <- unique(unlist(STANi3.HIERARCHYINV[x]))
#' data.all <- data.all[data.all$cou%in%as.character(unique(est.array.m$cou)) &
#'                      data.all$var%in%unique(est.array.m$var) &
#'                      data.all$ind%in%union(unique(est.array.m$ind), ind.parent) &
#'                      data.all$year%in%nameyear &
#'                      data.all$sou%in%unique(unlist(strsplit(as.character(est.array.m$value), split = ", "))),]
#'
#' res.ext <- NULL
#' data.ext <- data.all
#' print(est.array.d[est.array.d$est=="EXT",])
#' res.ext <- estimate(data=data.ext,
#'                     sources=est.array.d[est.array.d$est=="EXT",],
#'                     period=nameyear)
#' ## add back rows from main source
#' sou.main.ext <- melt(ui.stani3Estimate.est.array[exportcou, exportvar, nameind, "EXT", "MAIN"])
#' names(sou.main.ext) <- sub("value", "sou", names(sou.main.ext))
#' data.all.ext.main <- merge(data.all, sou.main.ext)
#' res.ext <- rbind(res.ext, data.all.ext.main) # adds around 6000 rows from data.all
#' res.ext <- res.ext[!duplicated(res.ext[,!colnames(res.ext)%in%c("sou", "value")]),]
#' res.ext$sou <- "PATCHEXT"
#' res.ext <- rbind(res.ext, data.all)
#' ##
#' res.det <- NULL
#' data.det <- res.ext
#' print(est.array.d[est.array.d$est=="DET",])

#' res.det <- estimate(data=data.det
#'                     ,
#'                     sources=est.array.d[est.array.d$est=="DET",]
#'                     ,
#'                     period=nameyear
#'                     )
#'
#' res.det <- rbind(res.det, res.ext)
#' data.est <- rbind(res.det, res.ext)
#' data.est <- data.est[!duplicated(data.est[,!colnames(data.est)%in%c("sou", "value")]),]
#' data.est$ind <- factor(data.est$ind, levels = nameind)
#' data.est <- data.est[order(data.est$ind),]
#' data <- data.est
#'
#' estimate(data=data.all, sources=est.array.d[est.array.d$est=="DET",], period=nameyear)
#' estimate(data=data.all, sources=est.array.d, period=nameyear)

estimate <- function(data=data.all,
                     sources=est.array.d[est.array.d$est=="DET",],
                     period=c(1995:2010),
                     isic=3)
{

    if (isic==3) {
        hierarchyinv <- STANi3.HIERARCHYINV
    } else if (isic==4) {
        hierarchyinv <- STANi4.HIERARCHYINV
    }

    data.est <- NULL
    ## i <- 1
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
        ## sou.main; sou.sec; cou; var; ind; est
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
                        PATCHEXT <- stan::extend(
                            data = data.ext.start
                          ,
                            namesou = c(sou.main, sou.sec)
                        )
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
                ## bug: if ind / ind.peers in main source don't do anything
                PATCHDET <- NULL
                data.patch.det <- NULL
                data.patch.det.p <- NULL
                newsou <- "PATCHDET"
                ind.peers <- ind
                ## ind.parent <- as.character(STANi3.HIERARCHYINV[[ind]])
                ## STANi4.HIERARCHYINV[["D05T39"]]
                ind.parent <- as.character(hierarchyinv[[ind]])
                data.det.start <- data[data$cou==cou &
                                       data$sou%in%namesou &
                                       data$var==var &
                                       data$ind%in%c(ind.parent[1], ind.peers) &
                                       data$year%in%period &
                                       data$value!=0,]
                ## possibly introducing breaks!!
                data.det.start <- rbind(data.det.start, data.est[data.est$ind%in%ind.parent[1],])
                ## necessary?
                data.det.start$sou <- sub("PATCHDET", "PATCHEXT", data.det.start$sou)
                ##
                data.det.start <- data.det.start[!duplicated(data.det.start[,!colnames(data.det.start)%in%c("value")]),]
                ## check if data contains main source and secondary source contains ind
                if (nrow(data.det.start[data.det.start$sou==sou.main,]) > 0 &
                    ind%in%data.det.start$ind[data.det.start$sou==sou.sec]) {
                    ## if parent industry in main source: calculate using this number
                    if (ind.parent[1]%in%unique(data.det.start$ind[data.det.start$sou==sou.main])) {
                        PATCHDET <- try(stan::detail(data = data.det.start,
                                                     namesou = namesou,
                                                     ind.parent = ind.parent[1],
                                                     ind.peers = ind.peers),
                                        silent = TRUE)
                    } else {
                        ## if parent industry not in main source, look for level-up parent
                        p <- 2
                        parent.in.namesou <- FALSE
                        while (p==2 | (p <= length(ind.parent) & parent.in.namesou==FALSE)) {
                            data.det.start.p <- data[data$cou==cou &
                                                     data$sou%in%namesou &
                                                     data$var==var &
                                                     data$ind%in%ind.parent[1:p] &
                                                     data$year%in%period,]
                            ## rm(data.patch.det.p)
                            ##
                            if (!ind.parent[p-1]%in%unique(data.det.start.p$ind[data.det.start.p$sou==sou.sec])) {
                                print(paste(ind.parent[p-1], "missing in", sou.sec))
                            }
                            if (!ind.parent[p]%in%unique(data.det.start.p$ind[data.det.start.p$sou==sou.sec])) {
                                print(paste(ind.parent[p], "missing in", sou.sec))
                            }
                            ## if (ind.parent[p]%in%unique(data.det.start.p$ind[data.det.start.p$sou==sou.main])) {
                            if (ind.parent[p]%in%unique(data.det.start.p$ind[data.det.start.p$sou==sou.main]) &
                                ind.parent[p]%in%unique(data.det.start.p$ind[data.det.start.p$sou==sou.sec])) {
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
                            ## if (exists("data.patch.det.p")==TRUE) {
                            if (length(data.patch.det.p) > 0) {
                                parent.in.namesou <- ind.parent[p]%in%unique(data.patch.det.p$ind[data.patch.det.p$sou=="PATCHDET"])
                            } else {
                                parent.in.namesou <- FALSE
                            }
                            p = p + 1
                        }
                        if (length(data.patch.det.p) > 0) {
                            PATCHDET <- stan::detail(data = rbind(data.det.start, data.patch.det.p),
                                                     namesou = sub(sou.main, newsou, namesou),
                                                     ind.parent = ind.parent[1],
                                                     ind.peers = ind.peers)
                        }
                    }
                    ## if (!is.null(nrow(PATCHDET))) {
                    if (length(PATCHDET) > 0) {
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
                if (length(data.patch.det) > 0) {
                    data.patch.det <- data.patch.det[data.patch.det$ind==ind,]
                    data.patch.det <- data.patch.det[!is.infinite(data.patch.det$value) & data.patch.det$value!=0,]
                    data.est <- rbind(data.est, data.patch.det)
                }
            } # end if(est=="DET") { ... }
        }
    }

    return(data.est)
}

