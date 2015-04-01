#' Hierarchy
#'
#' Create STAN industry hierarchy
#'
#' Function to create an industry hierarchy based on a aggregation matrix with industries in columns and aggregates in rows. Membership is indicated with a '1'.
#'
#' @param file the aggregation matrix in csv format.
#' @param agg.exclude character vector with aggregates to be excluded from the hierarchy.
#' @param parent logical to list all parents of industry instead of members.
#' @param order logical to create ordered vector of aggregates and contained sectors
#'
#' @author OECD STAN
#' @keywords dimensions
#' @seealso \code{\link{indAggregate}}, \code{\link{loadDim}}
#' @export
#' @examples
#' hierarchy(file=system.file("extdata", "loadDim_indi3agg.csv", package = "stan"))

hierarchy <- function(file=system.file("extdata", "loadDim_indi3agg.csv", package = "stan"),
                      agg.exclude,
                      parent=FALSE,
                      order=FALSE)
{
    conv.stan <- read.csv(file)
    conv.stan <- conv.stan[!conv.stan$agg%in%agg.exclude,]
    conv.stan <- conv.stan[,!colnames(conv.stan)%in%c("contents")]
    ## create sorted hierarchy
    if (order==TRUE) {

        ind2d <- colnames(conv.stan)[-1]
        sorted.ind <- data.frame(agg = ind2d,
                                 ## first = seq(along = ind2d) + 1, # +1 because "agg" in first column,
                                 first = seq(along = ind2d), # remove first column from search because of C01T02 factor
                                 sum = rep(1, length(ind2d)))
        ## agg <- "C01T02"
        for (agg in conv.stan$agg) {
            conv.stan$first[conv.stan$agg==agg] <- match("1", conv.stan[conv.stan$agg==agg, -1]) # remove first column from search because of C01T02 factor
            conv.stan$sum[conv.stan$agg==agg] <- sum(conv.stan[conv.stan$agg==agg, c(2:(length(conv.stan)-1))])
        }
        sorted.agg <- conv.stan[colnames(conv.stan)%in%c("agg", "first", "sum")]
        sorted.ind.agg <- rbind(sorted.ind, sorted.agg)
        sorted.ind.agg <- sorted.ind.agg[order(sorted.ind.agg$first, -sorted.ind.agg$sum),]
        sorted.ind.agg$agg <- factor(sorted.ind.agg$agg, levels = sorted.ind.agg$agg)
        data.all <- sorted.ind.agg$agg

    } else {

        rownames(conv.stan) <- conv.stan[,"agg"]
        conv.stan <- conv.stan[,!colnames(conv.stan)%in%c("agg", "first", "sum")]
        conv.stan <- as.matrix(conv.stan)
        conv.stan <- t(conv.stan)
        ## for aggregates
        all.ind <- NULL
        all.ind.sum <- NULL
        all.parent <- NULL
        all.parent.sum <- NULL
        for (i in c(1:ncol(conv.stan))) {
            for (j in c(1:ncol(conv.stan))) {
                if (sum(conv.stan[,i]) > sum(conv.stan[,j])) {
                    diff <- conv.stan[,i] - conv.stan[,j]
                    if (all(diff >= 0)) {
                        all.ind <- c(all.ind, c(colnames(conv.stan)[j]))
                        all.ind.sum <- c(all.ind.sum, sum(conv.stan[,j]))
                        all.parent <- c(all.parent, colnames(conv.stan)[i])
                        all.parent.sum <- c(all.parent.sum, sum(conv.stan[,i]))
                    }
                }
                if (sum(conv.stan[,i]) < sum(conv.stan[,j])) {
                    diff <- -conv.stan[,i] + conv.stan[,j]
                    if (all(diff >= 0)) {
                        all.ind <- c(all.ind, c(colnames(conv.stan)[i]))
                        all.ind.sum <- c(all.ind.sum, sum(conv.stan[,i]))
                        all.parent <- c(all.parent, colnames(conv.stan)[j])
                        all.parent.sum <- c(all.parent.sum, sum(conv.stan[,j]))
                    }
                }
            }
        }
        ##
        ind.parent <- cbind.data.frame(all.ind, as.numeric(all.ind.sum), all.parent, as.numeric(all.parent.sum))
        names(ind.parent) <- c("ind", "rank.ind", "parent", "rank.parent")
        ind.parent <- ind.parent[order(ind.parent$rank.parent),]
        ind.parent <- ind.parent[!duplicated(ind.parent),]
        ## all contained industries
        data.all <- NULL
        for (agg in unique(colnames(conv.stan))) {
            data <- merge(c(rownames(conv.stan)[conv.stan[,match(agg, colnames(conv.stan))]==1]), agg, all = TRUE)
            data.all <- rbind(data.all, data)
        }
        ## data.all
        names(data.all) <- c("ind", "parent")
        ## identify smallest aggregates - aggregates that are not at the same time parents of other aggregates
        ind.parent.small <- data.all[data.all$parent%in%setdiff(ind.parent$ind, ind.parent$parent),]
        ## identify industries that are contained in an aggregate other than the smallest aggregates, e.g. C05 in C01T05
        ind.parent.other <- data.all[!data.all$parent%in%setdiff(ind.parent$ind, ind.parent$parent) &
                                     !data.all$ind%in%ind.parent.small$ind,]
        ## remove duplicates: use parent ranks
        ind.parent.other.rank <- merge(ind.parent.other, ind.parent[,colnames(ind.parent)%in%c("parent", "rank.parent")], by = "parent", all = FALSE)
        ind.parent.other.rank <- ind.parent.other.rank[order(ind.parent.other.rank$rank.parent),]
        ## each industry can only be in one aggregate
        ind.parent.other.rank <- ind.parent.other.rank[!duplicated(ind.parent.other.rank[,"ind"]),]
        ## prepare data
        data1 <- ind.parent.small       # small aggregates: autonomous
        data2 <- subset(ind.parent[!duplicated(ind.parent[,"ind"]),], select = names(data1)) # all aggregates
        data3 <- subset(ind.parent.other.rank, select = names(data1)) # 2-digits of other aggregates
        data.in <- rbind(data1, data2, data3)
        ## combine to list
        data.all <- NULL
        if (parent==TRUE) {
            for (ind in unique(data.in$ind)) {
                ind.parent <- data.in$parent[data.in$ind==ind]
                i = 1
                ind.parent.parent <- NULL
                while (i==1 | length(ind.parent.parent) > 0) { # Error: object 'ind.parent.parent' not found
                    ind.parent.parent <- data.in$parent[data.in$ind==as.character(ind.parent[i])]
                    ind.parent <- union(ind.parent, ind.parent.parent)
                    i = i + 1
                }
                data <- list(as.factor(ind.parent))
                names(data) <- ind
                data.all <- c(data.all, data)
            }
        } else {
            for (pr in unique(data.in$parent)) {
                data <- list(data.in$ind[data.in$parent==pr])
                names(data) <- pr
                data.all <- c(data.all, data)
            }
        }

    }
    ## return(data.all)
    return(lapply(data.all, as.character)) #
}

