#' Get estate
#'
#' Get real estate data
#'
#' This function scrapes data from \code{\link{pap.fr}} using the \code{XML} and
#' the \code{stringr} package.
#'
#' @param type choose to scrape offers for rent or sale.
#' @param pages select the number of pages, each of them containing
#' 40 items.
#'
#' @author Bo Werth
#' @keywords scrape
#' @seealso \code{www.pap.fr}
#' @export
#' @examples
#' data <- getEstate(type = "vente", pages = 50)

AGG.A88 <- function(data=NULL) {
    attach(data); if (c('D01T02')%in%colnames(data)) data$D01T02 <- D01T02 + D01 + D02 else data$D01T02 <- D01 + D02; detach(data)
    attach(data); if (c('D01T03')%in%colnames(data)) data$D01T03 <- D01T03 + D01T02 + D03 else data$D01T03 <- D01T02 + D03; detach(data)
    attach(data); if (c('D05T06')%in%colnames(data)) data$D05T06 <- D05T06 + D05 + D06 else data$D05T06 <- D05 + D06; detach(data)
    attach(data); if (c('D07T09')%in%colnames(data)) data$D07T09 <- D07T09 + D07 + D08 + D09 else data$D07T09 <- D07 + D08 + D09; detach(data)
    attach(data); if (c('D05T09')%in%colnames(data)) data$D05T09 <- D05T09 + D05T06 + D07T09 else data$D05T09 <- D05T06 + D07T09; detach(data)
    attach(data); if (c('D10T11')%in%colnames(data)) data$D10T11 <- D10T11 + D10 + D11 else data$D10T11 <- D10 + D11; detach(data)
    attach(data); if (c('D10T12')%in%colnames(data)) data$D10T12 <- D10T12 + D10T11 + D12 else data$D10T12 <- D10T11 + D12; detach(data)
    attach(data); if (c('D13T14')%in%colnames(data)) data$D13T14 <- D13T14 + D13 + D14 else data$D13T14 <- D13 + D14; detach(data)
    attach(data); if (c('D13T15')%in%colnames(data)) data$D13T15 <- D13T15 + D13T14 + D15 else data$D13T15 <- D13T14 + D15; detach(data)
    attach(data); if (c('D16T18')%in%colnames(data)) data$D16T18 <- D16T18 + D16 + D17 + D18 else data$D16T18 <- D16 + D17 + D18; detach(data)
    attach(data); if (c('D20T21')%in%colnames(data)) data$D20T21 <- D20T21 + D20 + D21 else data$D20T21 <- D20 + D21; detach(data)
    attach(data); if (c('D22T23')%in%colnames(data)) data$D22T23 <- D22T23 + D22 + D23 else data$D22T23 <- D22 + D23; detach(data)
    attach(data); if (c('D19T23')%in%colnames(data)) data$D19T23 <- D19T23 + D19 + D20T21 + D22T23 else data$D19T23 <- D19 + D20T21 + D22T23; detach(data)
    attach(data); if (c('D24T25')%in%colnames(data)) data$D24T25 <- D24T25 + D24 + D25 else data$D24T25 <- D24 + D25; detach(data)
    attach(data); if (c('D26T27')%in%colnames(data)) data$D26T27 <- D26T27 + D26 + D27 else data$D26T27 <- D26 + D27; detach(data)
    attach(data); if (c('D26T28')%in%colnames(data)) data$D26T28 <- D26T28 + D26T27 + D28 else data$D26T28 <- D26T27 + D28; detach(data)
    attach(data); if (c('D29T30')%in%colnames(data)) data$D29T30 <- D29T30 + D29 + D30 else data$D29T30 <- D29 + D30; detach(data)
    attach(data); if (c('D31T32')%in%colnames(data)) data$D31T32 <- D31T32 + D31 + D32 else data$D31T32 <- D31 + D32; detach(data)
    attach(data); if (c('D31T33')%in%colnames(data)) data$D31T33 <- D31T33 + D31T32 + D33 else data$D31T33 <- D31T32 + D33; detach(data)
    attach(data); if (c('D10T33')%in%colnames(data)) data$D10T33 <- D10T33 + D10T12 + D13T15 + D16T18 + D19T23 + D24T25 + D26T28 + D29T30 + D31T33 else data$D10T33 <- D10T12 + D13T15 + D16T18 + D19T23 + D24T25 + D26T28 + D29T30 + D31T33; detach(data)
    attach(data); if (c('D37T39')%in%colnames(data)) data$D37T39 <- D37T39 + D37 + D38 + D39 else data$D37T39 <- D37 + D38 + D39; detach(data)
    attach(data); if (c('D36T39')%in%colnames(data)) data$D36T39 <- D36T39 + D36 + D37T39 else data$D36T39 <- D36 + D37T39; detach(data)
    attach(data); if (c('D35T39')%in%colnames(data)) data$D35T39 <- D35T39 + D35 + D36T39 else data$D35T39 <- D35 + D36T39; detach(data)
    attach(data); if (c('D41T43')%in%colnames(data)) data$D41T43 <- D41T43 + D41 + D42 + D43 else data$D41T43 <- D41 + D42 + D43; detach(data)
    attach(data); if (c('D45T47')%in%colnames(data)) data$D45T47 <- D45T47 + D45 + D46 + D47 else data$D45T47 <- D45 + D46 + D47; detach(data)
    attach(data); if (c('D49T53')%in%colnames(data)) data$D49T53 <- D49T53 + D49 + D50 + D51 + D52 + D53 else data$D49T53 <- D49 + D50 + D51 + D52 + D53; detach(data)
    attach(data); if (c('D55T56')%in%colnames(data)) data$D55T56 <- D55T56 + D55 + D56 else data$D55T56 <- D55 + D56; detach(data)
    attach(data); if (c('D45T56')%in%colnames(data)) data$D45T56 <- D45T56 + D45T47 + D49T53 + D55T56 else data$D45T56 <- D45T47 + D49T53 + D55T56; detach(data)
    attach(data); if (c('D59T60')%in%colnames(data)) data$D59T60 <- D59T60 + D59 + D60 else data$D59T60 <- D59 + D60; detach(data)
    attach(data); if (c('D58T60')%in%colnames(data)) data$D58T60 <- D58T60 + D58 + D59T60 else data$D58T60 <- D58 + D59T60; detach(data)
    attach(data); if (c('D62T63')%in%colnames(data)) data$D62T63 <- D62T63 + D62 + D63 else data$D62T63 <- D62 + D63; detach(data)
    attach(data); if (c('D58T63')%in%colnames(data)) data$D58T63 <- D58T63 + D58T60 + D61 + D62T63 else data$D58T63 <- D58T60 + D61 + D62T63; detach(data)
    attach(data); if (c('D64T66')%in%colnames(data)) data$D64T66 <- D64T66 + D64 + D65 + D66 else data$D64T66 <- D64 + D65 + D66; detach(data)
    attach(data); if (c('D69T70')%in%colnames(data)) data$D69T70 <- D69T70 + D69 + D70 else data$D69T70 <- D69 + D70; detach(data)
    attach(data); if (c('D69T71')%in%colnames(data)) data$D69T71 <- D69T71 + D69T70 + D71 else data$D69T71 <- D69T70 + D71; detach(data)
    attach(data); if (c('D74T75')%in%colnames(data)) data$D74T75 <- D74T75 + D74 + D75 else data$D74T75 <- D74 + D75; detach(data)
    attach(data); if (c('D73T75')%in%colnames(data)) data$D73T75 <- D73T75 + D73 + D74T75 else data$D73T75 <- D73 + D74T75; detach(data)
    attach(data); if (c('D69T75')%in%colnames(data)) data$D69T75 <- D69T75 + D69T71 + D72 + D73T75 else data$D69T75 <- D69T71 + D72 + D73T75; detach(data)
    attach(data); if (c('D80T82')%in%colnames(data)) data$D80T82 <- D80T82 + D80 + D81 + D82 else data$D80T82 <- D80 + D81 + D82; detach(data)
    attach(data); if (c('D77T82')%in%colnames(data)) data$D77T82 <- D77T82 + D77 + D78 + D79 + D80T82 else data$D77T82 <- D77 + D78 + D79 + D80T82; detach(data)
    attach(data); if (c('D69T82')%in%colnames(data)) data$D69T82 <- D69T82 + D69T75 + D77T82 else data$D69T82 <- D69T75 + D77T82; detach(data)
    attach(data); if (c('D68T82')%in%colnames(data)) data$D68T82 <- D68T82 + D68 + D69T82 else data$D68T82 <- D68 + D69T82; detach(data)
    attach(data); if (c('D87T88')%in%colnames(data)) data$D87T88 <- D87T88 + D87 + D88 else data$D87T88 <- D87 + D88; detach(data)
    attach(data); if (c('D86T88')%in%colnames(data)) data$D86T88 <- D86T88 + D86 + D87T88 else data$D86T88 <- D86 + D87T88; detach(data)
    attach(data); if (c('D84T88')%in%colnames(data)) data$D84T88 <- D84T88 + D84 + D85 + D86T88 else data$D84T88 <- D84 + D85 + D86T88; detach(data)
    attach(data); if (c('D90T92')%in%colnames(data)) data$D90T92 <- D90T92 + D90 + D91 + D92 else data$D90T92 <- D90 + D91 + D92; detach(data)
    attach(data); if (c('D90T93')%in%colnames(data)) data$D90T93 <- D90T93 + D90T92 + D93 else data$D90T93 <- D90T92 + D93; detach(data)
    attach(data); if (c('D94T96')%in%colnames(data)) data$D94T96 <- D94T96 + D94 + D95 + D96 else data$D94T96 <- D94 + D95 + D96; detach(data)
    attach(data); if (c('D97T98')%in%colnames(data)) data$D97T98 <- D97T98 + D97 + D98 else data$D97T98 <- D97 + D98; detach(data)
    attach(data); if (c('D90T96')%in%colnames(data)) data$D90T96 <- D90T96 + D90T93 + D94T96 else data$D90T96 <- D90T93 + D94T96; detach(data)
    attach(data); if (c('D90T99')%in%colnames(data) & c('D99')%in%colnames(data)) data$D90T99 <- D90T99 + D90T96 + D97T98 + D99 else if (c('D90T99')%in%colnames(data) & !c('D99')%in%colnames(data)) data$D90T99 <- D90T99 + D90T96 + D97T98 else data$D90T99 <- D90T96 + D97T98; detach(data)
    attach(data); if (c('D84T99')%in%colnames(data)) data$D84T99 <- D84T99 + D84T88 + D90T99 else data$D84T99 <- D84T88 + D90T99; detach(data)
    attach(data); if (c('D05T39')%in%colnames(data)) data$D05T39 <- D05T39 + D05T09 + D10T33 + D35T39 else data$D05T39 <- D05T09 + D10T33 + D35T39; detach(data)
    attach(data); if (c('D45T82')%in%colnames(data)) data$D45T82 <- D45T82 + D45T56 + D58T63 + D64T66 + D68T82 else data$D45T82 <- D45T56 + D58T63 + D64T66 + D68T82; detach(data)
    attach(data); if (c('D45T99')%in%colnames(data)) data$D45T99 <- D45T99 + D45T82 + D84T99 else data$D45T99 <- D45T82 + D84T99; detach(data)
    attach(data); if (c('D45T82X')%in%colnames(data)) data$D45T82X <- D45T82X + D45T56 + D58T63 + D64T66 + D69T82 else data$D45T82X <- D45T56 + D58T63 + D64T66 + D69T82; detach(data)
    attach(data); if (c('D05T82X')%in%colnames(data)) data$D05T82X <- D05T82X + D05T39 + D41T43 + D45T82X else data$D05T82X <- D05T39 + D41T43 + D45T82X; detach(data)
    attach(data); if (c('D16A31')%in%colnames(data)) data$D16A31 <- D16A31 + D16 + D31T32 else data$D16A31 <- D16 + D31T32; detach(data)
    attach(data); if (c('D24T33X')%in%colnames(data)) data$D24T33X <- D24T33X + D24T25 + D26T28 + D29T30 + D33 else data$D24T33X <- D24T25 + D26T28 + D29T30 + D33; detach(data)
    attach(data); if (c('ENERGYP')%in%colnames(data)) data$ENERGYP <- ENERGYP + D05T06 + D19 + D35 else data$ENERGYP <- D05T06 + D19 + D35; detach(data)
    attach(data); if (c('NONMAN')%in%colnames(data)) data$NONMAN <- NONMAN + D01T03 + D05T09 + D35T39 + D41T43 + D45T99 else data$NONMAN <- D01T03 + D05T09 + D35T39 + D41T43 + D45T99; detach(data)
    attach(data); if (c('DTOTAL')%in%colnames(data)) data$DTOTAL <- DTOTAL + D10T33 + NONMAN else data$DTOTAL <- D10T33 + NONMAN; detach(data)
    attach(data); if (c('D99')%in%colnames(data)) data$D90T99 <- data$D90T96 + data$D97T98 + data$D99 else data$D90T99 <- D90T96 + D97T98; detach(data)
    return(data)
}
