#' Aggregate ISIC
#'
#' Aggregate industries ISIC Rev. 3 and ISIC Rev. 4
#'
#' Calculate STAN ISIC Rev. 3 aggregates based on the A60 list and STAN ISIC Rev. 4 aggregates based on the A88 list.
#'
#' @param data a dataframe having STAN industry labels (ISIC Rev. 3 or ISIC Rev. 4) as column names.
#' @param isic an integer specifying the ISIC classification of data sources.
#' @param cumulative logical to cumulate values if aggregate already existing in data.
#' @param naAsZero logical convert NAs to zero before calculating aggregates.
#' @param fill2D logical add column with NA or zero for each missing 2-digit industry
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
                         naAsZero=FALSE,
                         fill2D=FALSE
                         )
{
    if (isic==3)
    {
        list.agg <- STANi3.HIERARCHY
        list.agg.add <- list(## "C01T02" = c("C01", "C02"),
                             ## "C01T05" = c("C01T02", "C05"),
                             ## "C10T12" = c("C10", "C11", "C12"),
                             ## "C13T14" = c("C13", "C14"),
                             ## "C10T14" = c("C10T12", "C13T14"),
                             ## "C15T16" = c("C15", "C16"),
                             ## "C17T18" = c("C17", "C18"),
                             ## "C17T19" = c("C17T18", "C19"),
                             ## "C21T22" = c("C21", "C22"),
                             ## "C20T22" = c("C20", "C21T22"),
                             ## ## "C24" = c("C2423", "C24X"),
                             ## "C23T25" = c("C23", "C24", "C25"),
                             ## "C23T26" = c("C23T25", "C26"),
                             ## ## "C27" = c("C271T31", "C272T32"),
                             ## "C27T28" = c("C27", "C28"),
                             ## ## "C31" = c("C313", "C31X"),
                             ## ## "C32" = c("C321", "C322", "C323"),
                             ## ## "C33" = c("C3312", "C3313", "C33X"),
                             ## "C30T33X" = c("C30", "C32", "C33"),
                             ## "C30T33" = c("C30T33X", "C31"),
                             ## "C29T33" = c("C29", "C30T33"),
                             ## ## "C35" = c("C351", "C353", "C352A9"),
                             ## "C34T35" = c("C34", "C35"),
                             ## "C36T37" = c("C36", "C37"),
                             ## "C15T37" = c("C15T16", "C17T19", "C20T22", "C23T26", "C27T28", "C29T33"),
                             ## "C40T41" = c("C40", "C41"),
                             ## ## "C51" = c("C515", "C51X", "C52"),
                             ## "C50T52" = c("C50", "C51", "C52"),
                             ## "C50T55" = c("C50T52", "C55"),
                             ## ## "C64" = c("C641", "C642"),
                             ## "C60T63" = c("C60", "C61", "C62", "C63"),
                             ## "C60T64" = c("C60T63", "C64"),
                             ## "C65T67" = c("C65", "C66", "C67"),
                             ## ## "C71" = c("C7123", "C71X"),
                             ## ## "C74" = c("C741T3", "C749"),
                             ## "C73T74" = c("C73", "C74"),
                             ## "C71T74" = c("C71", "C72", "C73T74"),
                             ## "C70T74" = c("C70", "C71T74"),
                             ## "C65T74" = c("C65T67", "C70T74"),
                             ## "C90T93" = c("C90", "C91", "C92", "C93"),
                             ## "C75T99" = c("C75", "C80", "C85", "C90T93", "C95"),
                             ## "C75T99" = c("C75T99", "C99"),
                             "C20A36" = as.factor(c("C20", "C36")),
                             ## "C10T41" = as.factor(c("C10T14", "C15T37", "C40T41")),
                             "C27T35" = as.factor(c("C27T28", "C29T33", "C34T35")),
                             ## "C50T64" = as.factor(c("C50T55", "C60T64")),
                             "C50T74X" = as.factor(c("C50T64", "C65T67", "C71T74")),
                             ## "C50T74" = as.factor(c("C50T64", "C65T74")),
                             ## "C50T99" = as.factor(c("C50T74", "C75T99")),
                             ## "CTOTAL" = as.factor(c("C01T05", "C10T41", "C45", "C50T99")),
                             "C10T74X" = as.factor(as.factor(c("C10T41", "C45", "C50T74X"))),
                             "LOTECH" = as.factor(c("C15T16", "C17T19", "C20", "C21T22", "C36T37")),
                             "HMHTECH" = as.factor(c("C24", "C29T33", "C34T35")),
                             "ENERGYP" = as.factor(c("C10T12", "C23", "C40")),
                             "NONMAN" = as.factor(c("C01T05", "C10T14", "C40T41", "C45", "C50T99")))
        list.agg <- c(list.agg, list.agg.add)
    } else if (isic==4)
    {
        list.agg <- list("D01T02" = c("D01", "D02"),
                         "D01T03" = c("D01T02", "D03"),
                         "D05T06" = c("D05", "D06"),
                         "D07T08" = c("D07", "D08"),
                         "D05T09" = c("D05T06", "D07T08", "D09"),
                         "D10T11" = c("D10", "D11"),
                         "D10T12" = c("D10T11", "D12"),
                         "D13T14" = c("D13", "D14"),
                         "D13T15" = c("D13T14", "D15"),
                         "D16T18" = c("D16", "D17", "D18"),
                         "D20T21" = c("D20", "D21"),
                         "D22T23" = c("D22", "D23"),
                         "D19T23" = c("D19", "D20T21", "D22T23"),
                         "D24T25" = c("D24", "D25"),
                         "D26T27" = c("D26", "D27"),
                         "D26T28" = c("D26T27", "D28"),
                         "D29T30" = c("D29", "D30"),
                         "D31T32" = c("D31", "D32"),
                         "D31T33" = c("D31T32", "D33"),
                         "D10T33" = c("D10T12", "D13T15", "D16T18", "D19T23", "D24T25", "D26T28", "D29T30", "D31T33"),
                         "D37T39" = c("D37", "D38", "D39"),
                         "D36T39" = c("D36", "D37T39"),
                         "D35T39" = c("D35", "D36T39"),
                         "D41T43" = c("D41", "D42", "D43"),
                         "D45T47" = c("D45", "D46", "D47"),
                         "D49T53" = c("D49", "D50", "D51", "D52", "D53"),
                         "D55T56" = c("D55", "D56"),
                         "D45T56" = c("D45T47", "D49T53", "D55T56"),
                         "D59T60" = c("D59", "D60"),
                         "D58T60" = c("D58", "D59T60"),
                         "D62T63" = c("D62", "D63"),
                         "D58T63" = c("D58T60", "D61", "D62T63"),
                         "D64T66" = c("D64", "D65", "D66"),
                         "D69T70" = c("D69", "D70"),
                         "D69T71" = c("D69T70", "D71"),
                         "D74T75" = c("D74", "D75"),
                         "D73T75" = c("D73", "D74T75"),
                         "D69T75" = c("D69T71", "D72", "D73T75"),
                         "D80T82" = c("D80", "D81", "D82"),
                         "D77T82" = c("D77", "D78", "D79", "D80T82"),
                         "D69T82" = c("D69T75", "D77T82"),
                         "D68T82" = c("D68", "D69T82"),
                         "D87T88" = c("D87", "D88"),
                         "D86T88" = c("D86", "D87T88"),
                         "D84T88" = c("D84", "D85", "D86T88"),
                         "D90T92" = c("D90", "D91", "D92"),
                         "D90T93" = c("D90T92", "D93"),
                         "D94T96" = c("D94", "D95", "D96"),
                         "D97T98" = c("D97", "D98"),
                         "D90T96" = c("D90T93", "D94T96"),
                         "D90T99" = c("D90T96", "D97T98"),
                         ## if D99 present, include in D90T99
                         "D90T99" = c("D90T99", "D99"),
                         ##
                         "D84T99" = c("D84T88", "D90T99"),
                         "D05T39" = c("D05T09", "D10T33", "D35T39"),
                         "D45T82" = c("D45T56", "D58T63", "D64T66", "D68T82"),
                         "D45T99" = c("D45T82", "D84T99"),
                         "D45T82X" = c("D45T56", "D58T63", "D64T66", "D69T82"),
                         "D05T82X" = c("D05T39", "D41T43", "D45T82X"),
                         "D16A31" = c("D16", "D31T32"),
                         "D24T33X" = c("D24T25", "D26T28", "D29T30", "D33"),
                         "ENERGYP" = c("D05T06", "D19", "D35"),
                         "NONMAN" = c("D01T03", "D05T09", "D35T39", "D41T43", "D45T99"),
                         "DTOTAL" = c("D10T33", "NONMAN"))
    }

    sum.na <- function(x) {
        x[which(is.na(x))] <- 0
        return(sum(x))
    }

    ## add missing A88 (2-digit) industries
    if (fill2D==TRUE)
    {
        if (isic==3) {
            missing.2d <- STANi4.INDA88[!STANi3.INDA60%in%colnames(data)]
        } else if (isic==4) {
            missing.2d <- STANi4.INDA88[!STANi4.INDA88%in%colnames(data)]
        }
        if (naAsZero==TRUE) {
            m <- matrix(0, nrow(data), length(missing.2d))
        } else {
            m <- matrix(NA, nrow(data), length(missing.2d))
        }
        colnames(m) <- missing.2d
        data <- cbind.data.frame(data, m)
    }

    for (i in seq(along=list.agg))
    {
        agg <- names(list.agg[i])
        parts <- list.agg[[i]]
        if (all(is.element(parts, colnames(data))==TRUE))
        {
            if (!agg %in% colnames(data))
            {
                if (cumulative==TRUE) {
                    temp <- data[, colnames(data) %in% c(agg, parts)]
                } else {
                    temp <- data[, colnames(data) %in% parts]
                }
                if (naAsZero==TRUE) {
                    new.agg <- unname(apply(as.matrix(temp), 1, "sum.na"))
                } else {
                    new.agg <- unname(apply(as.matrix(temp), 1, "sum"))
                }
                data <- cbind(data, new.agg)
                colnames(data) <- sub("new.agg", agg, colnames(data))
            }
        }
    }
    return(data)
}
