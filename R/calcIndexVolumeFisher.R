#' calcIndexVolumeFisher
#'
#' \code{calcIndexVolumeFisher}: calculate volume series using nominal series and Fisher chained index series
#'
#' calcIndexVolumeFisher(data, value.var, index.var, refyear)
#'
#' @param data a dataframe with STAN variables in columns.
#' @param value.var STAN variable containing the nominal or quantity series, e.g. \code{VALU}
#' @param index.var STAN variable containing the chained index series, e.g. \code{VKOT}
#' @param price.var STAN variable containing the price series, e.g. \code{VALP}
#' @param pyp.var STAN variable containing the previous-year price series, e.g. \code{VKPY}
#' @param volume.var STAN variable containing the volume series, e.g. \code{VALK}
#' @param refyear integer to specify the reference year of the chained index series, e.g. \code{2005}
#'
#' @author OECD STAN
#' @keywords chaining
#' @rdname indices
#' @seealso \code{\link{estimate}}
#' @export
#' @examples
#' data.stan.d <- read.csv(file = system.file(file.path("extdata", "volumeFisherPivot.csv"), package = "stan"))
#' data.stan.d$VALK2 <- calcIndexVolumeFisher(data = data.stan.d, value.var="VALU", index.var="VKOT", refyear=2005)
#' data.stan.d$VALK2 <- calcPypVolume(data = data.stan.d, value.var="VALU", pyp.var="VKPY2", refyear=2005)
#' data.stan.d$VALP2 <- calcVolumePrice(data = data.stan.d, value.var="VALU", volume.var="VALK")
#' data.stan.d$VKPY2 <- calcPricePyp(data = data.stan.d, value.var="VALU", price.var="VALP")
#' data.stan.d$VKPY2 <- calcVolumePyp(data = data.stan.d, value.var="VALU", volume.var="VALK2")

calcIndexVolumeFisher <- function(
    data=stop('"data" must be specified'),
    value.var=stop('"value.var" must be specified'),
    index.var=stop('"index.var" must be specified'),
    refyear=stop('"refyear" must be specified'))
{
    value.ts <- stats::ts(data=data[[value.var]], start=min(data$year), end = max(data$year))
    lagvalue.ts <- stats::lag(value.ts)
    index.ts <- stats::ts(data=data[[index.var]], start=min(data$year), end = max(data$year))
    lagindex.ts <- stats::lag(index.ts)
    l_py <- lagvalue.ts * index.ts / lagindex.ts # lag&val * &volume / lag&volume
    p_py <- value.ts * lagindex.ts / index.ts # &val * lag&volume / &volume
    f1 <- ((l_py / lagvalue.ts) * (value.ts / p_py)) ^.5 # ((L_&volume._PY / lag&val) * (&val / P_&volume._PY))**(1/2) ;
    f1[1] <- 1
    f2 <- f1
    for (t in seq(along = f2)) f2[t] <- prod(f1[1:t])
    denom <- f2[stats::time(f2)==refyear]
    f3 <- f2 / denom
    f4 <- value.ts[stats::time(value.ts)==refyear] * f3
    ## return(f4)
    return(as.vector(f4))
}

#' calcIndexPypFisher
#'
#' \code{calcIndexPypFisher}: calculate previous year price series using nominal series and Fisher chained index series
#'
#' calcIndexPypFisher(data, value.var, index.var, refyear)
#'
#' @rdname indices
#' @export
calcIndexPypFisher <- function(
    data=stop('"data" must be specified'),
    value.var=stop('"value.var" must be specified'),
    index.var=stop('"index.var" must be specified'),
    refyear=stop('"refyear" must be specified'))
{
	## refyear doesn't need to be specified
    volume.ts <- calcIndexVolumeFisher(data=data, value.var=value.var, index.var=index.var, refyear=refyear)
    value.ts <- stats::ts(data=data[[value.var]], start=min(data$year), end = max(data$year))
    lagvalue.ts <- stats::lag(value.ts)
    lagvolume.ts <- stats::lag(volume.ts)
    pyp.ts <- lagvalue.ts * volume.ts / lagvolume.ts
    return(as.vector(pyp.ts))
}

#' calcVolumePyp
#'
#' \code{calcVolumePyp}: calculate previous year price series using nominal series and volume series
#'
#' calcVolumePyp(data, value.var, index.var, refyear)
#'
#' @rdname indices
#' @export
calcVolumePyp <- function(
    data=stop('"data" must be specified'),
    value.var=stop('"value.var" must be specified'),
    volume.var=stop('"volume.var" must be specified'))
{
    value.ts <- stats::ts(data=data[[value.var]], start=min(data$year), end = max(data$year))
    volume.ts <- stats::ts(data=data[[volume.var]], start=min(data$year), end = max(data$year))
    if (length(value.ts) <= 2 | length(volume.ts) <=2)
        ## stop("less then two observations in time series")
        return()
    lagvalue.ts <- stats::lag(value.ts)
    lagvolume.ts <- stats::lag(volume.ts)
    pyp.ts <- lagvalue.ts * volume.ts / lagvolume.ts
    return(as.vector(pyp.ts))
}

#' calcPypVolume
#'
#' \code{calcPypVolume}: calculate volume series using nominal series and previous year price series
#'
#' calcPypVolume(data, value.var, index.var, refyear)
#'
#' @rdname indices
#' @export
calcPypVolume <- function(
    data=stop('"data" must be specified'),
    value.var=stop('"value.var" must be specified'),
    pyp.var=stop('"pyp.var" must be specified'),
    refyear=stop('"refyear" must be specified'))
{
    value.ts <- stats::ts(data=data[[value.var]], start=min(data$year), end = max(data$year))
    pyp.ts <- stats::ts(data=data[[pyp.var]], start=min(data$year), end = max(data$year))
    lagvalue.ts <- stats::lag(value.ts)
    l_py1 <- pyp.ts / lagvalue.ts
    l_py1[1] <- 1
    ## l_py1[length(l_py1[is.na(l_py1)])] <- 1 # set last NA value to 1
    ## l_py1[is.na(l_py1)] <- 1 # set last NA value to 1
    l_py2 <- l_py1
    for (t in seq(along = l_py2)) l_py2[t] <- prod(l_py1[1:t])
    denom <- l_py2[stats::time(l_py2)==refyear]
    l_py3 <- l_py2 / denom
    l_py4 <- value.ts[stats::time(value.ts)==refyear] * l_py3
    ## l_py4[is.na(pyp.ts)] <- NA
    return(as.vector(l_py4))
}

#' calcVolumePrice
#'
#' \code{calcVolumePrice}: calculate price series using nominal series and volume series
#'
#' calcVolumePrice(data, value.var, index.var, refyear)
#'
#' @rdname indices
#' @export
calcVolumePrice <- function(
    data=stop('"data" must be specified'),
    value.var=stop('"value.var" must be specified'),
    volume.var=stop('"volume.var" must be specified'))
{
    value.ts <- stats::ts(data=data[[value.var]], start=min(data$year), end = max(data$year))
    volume.ts <- stats::ts(data=data[[volume.var]], start=min(data$year), end = max(data$year))
    price.ts <- value.ts / volume.ts
    return(as.vector(price.ts))
}

#' calcPricePyp
#'
#' \code{calcPricePyp}: calculate previous year price series using nominal series and price series
#'
#' calcPricePyp(data, value.var, index.var, refyear)
#'
#' @rdname indices
#' @export
calcPricePyp <- function(
    data=stop('"data" must be specified'),
    value.var=stop('"value.var" must be specified'),
    price.var=stop('"price.var" must be specified'))
{
    value.ts <- stats::ts(data=data[[value.var]], start=min(data$year), end = max(data$year))
    price.ts <- stats::ts(data=data[[price.var]], start=min(data$year), end = max(data$year))
    lagprice.ts <- stats::lag(price.ts)
    relprice.ts <- price.ts / lagprice.ts
    pyp.ts <- value.ts / relprice.ts
    return(as.vector(pyp.ts))
}
