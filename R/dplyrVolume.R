#' cpIdxClGeom
#'
#' \code{cpIdxClGeom}: calculate volume series using nominal series and Fisher chained index series
#'
#' cpIdxClGeom(data, value.var, var.idx, refyear)
#'
#' @param data a dataframe with STAN variables in columns.
#' @param var.cp STAN variable containing the nominal or quantity series, e.g. \code{VALU}
#' @param var.idx STAN variable containing the chained index series, e.g. \code{VKOT}
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
#' data.stan.d$VALK <- cpIdxCl(data=data.stan.d,
#'                             var.cp="VALU",
#'                             var.idx="VKOT",
#'                             id.vars="ind",
#'                             refyear=2005)
#' var.trans <- data.frame(var.cp  = c("VALU", "PROD", "INTI", "GFCF", "CAPN", "CAPG"),
#'                         var.idx = c("VKOT", "PKOT", "IKOT", "GKOT", "CKOT", "CGOT"),
#'                         var.cl  = c("VALK", "PRDK", "INTK", "GFCK", "CPNK", "CPGK"),
#'                         var.pyp = c("VKPY", "PKPY", "IKPY", "GKPY", "CNPY", "CGPY"))

cpIdxClGeom <- function(data=stop("'data' must be specified"),
                          var.cp="VALU",
                          var.idx="VKOT",
                          id.vars="ind",
                          refyear=2005) {

    ## see: http://stackoverflow.com/questions/21208801/group-by-multiple-columns-in-dplyr-using-string-vector-input
    id.vars2 <- lapply(id.vars, as.symbol)

    call <- substitute(data.new <- data %>%
                           group_by_(.dots = id.vars) %>%
                               mutate(l_py = lag(var.cp) * var.idx / lag(var.idx),
                                      p_py = var.cp * lag(var.idx) / var.idx,
                                      f1 = ifelse((!is.na(var.cp) & is.na(l_py)), 1, ((l_py / lag(var.cp)) * (var.cp / p_py)) ^.5),
                                      f2 = cumprod(f1),
                                      f3 = f2 / f2[year==var.year],
                                      var.cl = var.cp[year==var.year] * f3
                                      )
                      ,
                       list(id.vars = id.vars2,
                            var.cp = as.name(var.cp),
                            var.idx = as.name(var.idx),
                            var.year = refyear))
    eval(call)
    return(as.vector(data.new$var.cl))
}

#' cpClPyp
#'
#' \code{cpVolPyp}: calculate previous year price series using nominal series and Fisher chained index series
#'
#' cpVolPyp(data, var.cp, var.cl, id.vars)
#'
#' @rdname indices
#' @export
cpVolPyp <- function(data=stop("'data' must be specified"),
                     var.cp="VALU",
                     var.cl="VALK",
                     id.vars="ind") {

    id.vars2 <- lapply(id.vars, as.symbol)

    call <- substitute(data.new <- data %>%
                           ## group_by(id.vars) %>%
                           group_by_(.dots = id.vars) %>%
                               mutate(var.pyp = lag(var.cp) * var.cl / lag(var.cl))
                      ,
                       list(id.vars = id.vars2,
                            var.cp = as.name(var.cp),
                            var.cl = as.name(var.cl)
                            ))
    eval(call)
    return(as.vector(data.new$var.pyp))
}

#' cpIdxPypGeom
#'
#' \code{cpIdxPypGeom}: calculate previous year price series using nominal series and Fisher chained index series
#'
#' cpIdxPypGeom(data, var.cp, var.idx, id.vars, refyear)
#'
#' @rdname indices
#' @export
cpIdxPypGeom <- function(data=stop("'data' must be specified"),
                             var.cp="VALU",
                             var.idx="VKOT",
                             id.vars="ind",
                             refyear=2005) {
    ## calculate volume series
    data$var.cl <- stan::cpIdxClGeom(data=data,
                                     var.cp=var.cp,
                                     var.idx=var.idx,
                                     id.vars=id.vars,
                                     refyear=refyear)
    ## calculate pyp series using calculated volume series
    data$var.pyp <- cpVolPyp(data=data,
                             var.cp=var.cp,
                             var.cl="var.cl",
                             id.vars=id.vars)
    return(as.vector(data$var.pyp))
}
