#' Rename Data
#'
#' Modify names of objects contained in in \code{rdata} file
#'
#' Conveniently rename existing objects within an \code{rdata} file
#'
#' @param file the exported \code{rdata} file.
#' @param from character vector with old dataset names.
#' @param to character vector with new dataset names.
#'
#' @author OECD STAN
#' @keywords package
#' @seealso \code{\link{packageData}}
#' @export
#' @examples
#' renameData(file = file.path(dbpath, "GitHub", "stanData", "data", "STANNAi3.rda"), from = c("DATA.STAN", "DATA.BTD"), to = c("DATA.STANi3", "DATA.BTDi3"))

renameData <- function(file=file.path(dbpath, "GitHub", "stanData", "data", "STANNAi3.rda"),
                       from=c("DATA.STAN", "DATA.BTD"),
                       to=c("DATA.STANi3", "DATA.BTDi3")
                       ) {
    env <- new.env()
    load(file, envir = env)
    list.exist <- ls(env)

    if (length(from)!=length(to)) {
        stop(paste('"from" and "to" do not have the same length'))
        return()
    }
    ## i <- 1
    for (i in seq(along=from)) {
        if (from[i]%in%list.exist) {
            eval(parse(text=paste0('env$', to[i], ' <- env$', from[i])))
            eval(parse(text=paste0('rm(', from[i], ', envir = env)')))
        } else {
            warning(paste(from[i], "not existing in", file))
        }
    }

    save(list = ls(env), file = file, envir = env)

}
