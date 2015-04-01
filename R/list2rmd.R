#' Read Metadata
#'
#' Read metadata from xls saved as tab delimited txt file
#'
#' Create list with non-empty metadata from file
#'
#' @param list nested list with two levels
#' @param label list containing labels for names of 'list'
#' @param formula list containing formulas for name of 'list'
#' @param print print code for list creation to console
#' @param file the exported \code{rdata} file.
#'
#' @author OECD STAN
#' @keywords metadata
#' @seealso \code{\link{packageData}}
#' @export
#' @examples
#' ## see /data-raw/create_vignette/stanIndicator.R
#' file <- file.path(dbpath, "GitHub", "jekyll", "industry", "data_stan_notes.rmd")

list2rmd <- function(list=stop("'list' must be specified"),
                     label=NULL,
                     formula=NULL,
                     text.prepend=NULL,
                     print=FALSE,
                     file=NULL) {

    text.body.all <- NULL
    for (level1 in names(list)) {
        id <- gsub("[.]", "", level1)
        text.body.level1 <- NULL

        if (!is.null(formula)) {

            text.formula <- paste0('$$',
                                   '\\rm{', id, '}_i = ',
                                   formula[[as.character(id)]],
                                   '$$')

            text.body.level1.formula <- paste0("Calculation\n",
                                               ':   ', text.formula, '\n',
                                               '\n', collapse="")

            ## text.body.level1.formula <- paste0(text.formula, '\n\n')

            text.body.level1 <- paste0(text.body.level1, text.body.level1.formula)
        }

        for (level2 in names(list[[level1]])) {
            text.vector <- unlist(list[[level1]][[level2]])
            if (length(text.vector) > 0) {
                text.body.level1.section <- paste0(level2, '\n',
                                                   ':   ', text.vector, '\n',
                                                   '\n', collapse="")

                text.body.level1 <- paste0(text.body.level1, text.body.level1.section)
            }
        }

        ## id <- gsub("[.]", "", level1)
        if (is.null(label))
            id.label <- id
        else
            id.label <- label[[as.character(id)]]

        text.body.all <- paste0(text.body.all,
                                '\n\n## ', id, ' {#', tolower(id), '}\n\n', id.label, '\n\n',
                                text.body.level1)
    }

    if (!is.null(text.prepend))
        text.body.all <- paste0(text.prepend, text.body.all)

    if (print==TRUE)
        return(cat(text.body.all))
    else
        if (is.null(file))
            return(cat("'file' must be specified"))

    fileCon <- file(file)
    writeLines(text = text.body.all, con = fileCon)
    close(fileCon)

    return(cat(paste0(file, '\n')))

}
