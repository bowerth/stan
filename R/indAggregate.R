#' Aggregate ISIC
#'
#' Aggregate industries ISIC Rev. 3 and ISIC Rev. 4
#'
#' Calculate STAN ISIC Rev. 3 aggregates based on the A60 list and STAN ISIC Rev. 4 aggregates based on the A88 list.
#'
#' @param data a dataframe having STAN industry labels (ISIC Rev. 3 or ISIC Rev. 4) as column names.
#' @param isic an integer specifying the ISIC classification of data sources.
#' @param cumulative logical to cumulate values if aggregate already existing in data.
#' @param naAsZero character vector to specify industries where NAs are treated as zero, \code{ALL} for all industries.
#' @param fill2D logical add column with NA or zero for each missing 2-digit industry
#' @param missing.2d character vector with industries to impute zero, \code{NULL} for all 2-digit
#' @param part logical industry codes in column names contain partial information, e.g. \code{D01T03part}. Use with \code{fill2D=TRUE} in case of part aggregates and some 2-digits
#'
#' @author OECD STAN
#' @keywords dimensions
#' @seealso \code{\link{loadDim}}
#' @export
#' @examples
#' ## ISIC Rev. 3:
#' x <- data.frame("cou"="JPN",
#'                 "year"=c(rep(1995, 4), rep(1996,4)),
#'                 "ind"=c("C01", "C02", "C05", "C10"),
#'                 "value"=c(10, 10, 10, 10))
#' data <- dcast(x,cou + year ~ ind, value.var='value')
#' data.agg <- indAggregate(data=data, isic=3)
#' ## ISIC Rev. 4:
#' x <- data.frame("cou"="JPN",
#'                 "year"=c(rep(1995, 4), rep(1996,4)),
#'                 "ind"=c("D01", "D02", "D03", "D05"),
#'                 "value"=c(10, 10, 10, 10))
#' data <- dcast(x,cou + year ~ ind, value.var='value')
#' data.agg <- indAggregate(data=data, isic=4)

indAggregate <- function(data=stop("'data' must be specified"),
                         isic=3,
                         cumulative=FALSE,
                         ## naAsZero=FALSE,
                         naAsZero=NULL,
                         fill2D=FALSE,
                         missing.2d=NULL, # c("C95", "C99")
                         part=FALSE,
                         print.missing=FALSE
                         )
{

    if (isic==3) {

        list.agg <- STANi3.HIERARCHY
        list.agg.add <- list("C20A36" = as.factor(c("C20", "C36")),
                             "C27T35" = as.factor(c("C27T28", "C29T33", "C34T35")),
                             "C50T74X" = as.factor(c("C50T64", "C65T67", "C71T74")),
                             "C10T74X" = as.factor(as.factor(c("C10T41", "C45", "C50T74X"))),
                             "LOTECH" = as.factor(c("C15T16", "C17T19", "C20", "C21T22", "C36T37")),
                             "HMHTECH" = as.factor(c("C24", "C29T33", "C34T35")),
                             "ENERGYP" = as.factor(c("C10T12", "C23", "C40")),
                             "NONMAN" = as.factor(c("C01T05", "C10T14", "C40T41", "C45", "C50T99"))
                             )

        list.agg <- c(list.agg, list.agg.add)

    } else if (isic==4) {

        list.agg <- STANi4.HIERARCHY
        list.agg.add <- list(
            ## "D01T02" = c("D01", "D02"),
            ## "D01T03" = c("D01T02", "D03"),
            ## "D05T06" = c("D05", "D06"),
            ## "D07T08" = c("D07", "D08"),
            ## "D05T09" = c("D05T06", "D07T08", "D09"),
            ## "D10T11" = c("D10", "D11"),
            ## "D10T12" = c("D10T11", "D12"),
            ## "D13T14" = c("D13", "D14"),
            ## "D13T15" = c("D13T14", "D15"),
            ## "D16T18" = c("D16", "D17", "D18"),
            ## "D20T21" = c("D20", "D21"),
            ## "D22T23" = c("D22", "D23"),
            ## "D19T23" = c("D19", "D20T21", "D22T23"),
            ## "D24T25" = c("D24", "D25"),
            ## "D26T27" = c("D26", "D27"),
            ## "D26T28" = c("D26T27", "D28"),
            ## "D29T30" = c("D29", "D30"),
            ## "D31T32" = c("D31", "D32"),
            ## "D31T33" = c("D31T32", "D33"),
            ## "D10T33" = c("D10T12", "D13T15", "D16T18", "D19T23", "D24T25", "D26T28", "D29T30", "D31T33"),
            ## "D37T39" = c("D37", "D38", "D39"),
            ## "D36T39" = c("D36", "D37T39"),
            ## "D35T39" = c("D35", "D36T39"),
            ## "D41T43" = c("D41", "D42", "D43"),
            ## "D45T47" = c("D45", "D46", "D47"),
            ## "D49T53" = c("D49", "D50", "D51", "D52", "D53"),
            ## "D55T56" = c("D55", "D56"),
            ## "D45T56" = c("D45T47", "D49T53", "D55T56"),
            ## "D59T60" = c("D59", "D60"),
            ## "D58T60" = c("D58", "D59T60"),
            ## "D62T63" = c("D62", "D63"),
            ## "D58T63" = c("D58T60", "D61", "D62T63"),
            ## "D64T66" = c("D64", "D65", "D66"),
            ## "D69T70" = c("D69", "D70"),
            ## "D69T71" = c("D69T70", "D71"),
            ## "D74T75" = c("D74", "D75"),
            ## "D73T75" = c("D73", "D74T75"),
            ## "D69T75" = c("D69T71", "D72", "D73T75"),
            ## "D80T82" = c("D80", "D81", "D82"),
            ## "D77T82" = c("D77", "D78", "D79", "D80T82"),
            ## "D69T82" = c("D69T75", "D77T82"),
            ## "D68T82" = c("D68", "D69T82"),
            ## "D87T88" = c("D87", "D88"),
            ## "D86T88" = c("D86", "D87T88"),
            ## "D84T88" = c("D84", "D85", "D86T88"),
            ## "D90T92" = c("D90", "D91", "D92"),
            ## "D90T93" = c("D90T92", "D93"),
            ## "D94T96" = c("D94", "D95", "D96"),
            ## "D97T98" = c("D97", "D98"),
            ## "D90T96" = c("D90T93", "D94T96"),
            ## "D90T99" = c("D90T96", "D97T98"),
            ## "D90T99" = c("D90T99", "D99"), # if D99 present, include in D90T99
            ## "D84T99" = c("D84T88", "D90T99"),
            "D05T39" = c("D05T09", "D10T33", "D35T39"),
            "D45T82" = c("D45T56", "D58T63", "D64T66", "D68T82"),
            "D45T99" = c("D45T82", "D84T99"),
            "D45T82X" = c("D45T56", "D58T63", "D64T66", "D69T82"),
            "D05T82X" = c("D05T39", "D41T43", "D45T82X"),
            "D16A31" = c("D16", "D31T32"),
            "D24T33X" = c("D24T25", "D26T28", "D29T30", "D33"),
            "ENERGYP" = c("D05T06", "D19", "D35"),
            "NONMAN" = c("D01T03", "D05T09", "D35T39", "D41T43", "D45T99")
            ## , # add other TECH groups
            ## "HITECH" = c(),
            ## "MHTECH" = c(),
            ## "MLTECH" = c(),
            ## "LOTECH" = c(),
            ## "HMHTECH" = c()
          , # SSIS
            "D241T31" = c("D241", "D2431"),
            "D242T32" = c("D242", "D2432"),
            "D302A9" = c("D302", "D309")
            ## , # should be differences, currently only summing possible
            ## "D25X" = c("D25", "D252"),
            ## "D26X" = c("D26", "D262"),
            ## "D32X" = c("D32", "D325"),
            ## "D46X" = c("D46", "D466")
        ## ,
        ## "DTOTAL" = c("D10T33", "NONMAN")
        )

        ## setdiff(names(list.agg.add), names(STANi4.HIERARCHY))

        list.agg <- c(list.agg, list.agg.add)

    }

    ## if (length(naAsZero) > 0) {
    if (length(naAsZero) == 1) {
        if (tolower(naAsZero) == "all") naAsZero = names(list.agg)
    }

    sum.na <- function(x) {
        x[which(is.na(x))] <- 0
        return(sum(x))
    }

    na.zero <- function(x) {
        x[which(is.na(x))] <- 0
        return(x)
    }

    ## names(data)
    ## x <- "D84T88part"
    ## if "part" industry is an aggregate, preserve "part" string, otherwise remove
    treatpart <- function (x, list.agg) {
        if (sub("part", "", x)%in%names(list.agg)) {
            return(x)
        } else {
            return(sub("part", "", x))
        }
    }
    if (part==TRUE) {
        names(data) <- sapply(names(data), treatpart, list.agg)
    }

    ## add missing A88 (2-digit) industries
    if (fill2D==TRUE) {
        if (length(missing.2d)==0) {
            if (isic==3) {
                missing.2d <- STANi3.INDA60[!STANi3.INDA60%in%colnames(data)]
            } else if (isic==4) {
                missing.2d <- STANi4.INDA88[!STANi4.INDA88%in%colnames(data)]
            }

        } else { # if (length(missing.2d) > 0)
            ## ignores if exists for some country
            missing.2d <- missing.2d[!missing.2d%in%colnames(data)]
        }
        ## if (naAsZero==TRUE) {
        ## if (length(naAsZero) > 0) {
        ##     m <- matrix(0, nrow(data), length(missing.2d))
        ## } else {
            m <- matrix(NA, nrow(data), length(missing.2d))
        ## }
        colnames(m) <- missing.2d
        data <- cbind.data.frame(data, m)
        ## replace NA in existing columns with zero
        if(length(naAsZero) > 0) {
            naAsZero.exist <- naAsZero[naAsZero%in%colnames(data)]
            data[, naAsZero.exist] <- unname(apply(as.matrix(data[, naAsZero.exist]), 2, FUN="na.zero"))
        }
    }

    ## names(list.agg)
    ## match("D62T63", names(list.agg))
    ## i <- 42 # D84T88
    ## i <- 19 # D62T63
    ## i <- 1

    missing.ind <- NULL

    for (i in seq(along=list.agg)) {

        agg <- names(list.agg[i])
        parts <- list.agg[[i]]

        if (all(is.element(parts, colnames(data))==TRUE)) {

            if (cumulative==TRUE) { # cumulative: add aggregate to itself
                temp <- data[, colnames(data) %in% c(agg, parts)]
                data <- data[, !colnames(data)==agg] # remove agg from data

            } else if (part==TRUE) { # add partial aggregate to aggregate
                temp <- data[, colnames(data) %in% c(paste0(agg, 'part'), parts)]
                names(temp) <- sub("part", "", names(temp))

            } else {
                temp <- data[, colnames(data) %in% parts]

            }

            new.agg <- unname(apply(as.matrix(temp), 1, "sum")) # cumulative==TRUE: aggregate included in temp
            ## list aggregates for which NAs are counted as zero
            if (length(naAsZero) > 0) {
                if (agg %in% naAsZero) {
                    new.agg <- unname(apply(as.matrix(temp), 1, "sum.na"))
                    ## put NA where all industries evaluate missing
                    bool1 <- unname(apply(as.matrix(temp), 1, "is.na"))
                    new.agg[unname(apply(as.matrix(bool1), 2, "all"))] <- NA
                }
            }
            if (!agg %in% colnames(data)) { # add new aggregate if not yet in data
                data <- cbind(data, new.agg)
                colnames(data) <- sub("new.agg", agg, colnames(data))
            } else {
                ## replace NA in existing aggregate with calculated numbers
                data[is.na(data[[agg]]), colnames(data)==agg] <- new.agg[is.na(data[[agg]])]
            }

          ## } new: print list of missing industries for each aggregate
          } else {
            if (!agg %in% colnames(data)) {
              missing.ind.agg <- list(parts[!parts%in%colnames(data)])
              names(missing.ind.agg) <- agg
              missing.ind <- c(missing.ind, missing.ind.agg)
            }
          }

    }

    if (print.missing==TRUE) return(missing.ind)

    return(data)
}

#' listMissInd
#'
#' \code{listMissInd}: for each missing aggregate, list missing industries. Return NULL if all industries are missing for an aggregate
#'
#' listMissInd(data, isic)
#;
#' @rdname indAggregate
#' @export
listMissInd <- function(data,
                        isic,
                        drop=FALSE
                        ) {

  if (isic==3) hierarchy <- STANi3.HIERARCHY
  if (isic==4) hierarchy <- STANi4.HIERARCHY

  ## list names after aggregation
  nameind <- 
    data %>%
      spread(key = ind, value = value) %>%
        stan::indAggregate(isic = isic) %>%
          names()
  
  ## select missing aggregates
  nameagg <- setdiff(names(hierarchy), nameind)

  missing_ind <-
    lapply(hierarchy[nameagg],
           function(x) {
             diff <- setdiff(x, nameind)
             if (length(diff)==length(x)) return()
             return(diff)
           }
           )

  if (drop) missing_ind <-
      missing_ind[sapply(missing_ind, length) > 0]

  return(missing_ind)
}
